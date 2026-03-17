(*
 * -----------------------------------------------------------------------------
 * Name               : McePosTable
 * Version            : 0.8.0
 * Date               : 2025-11-12
 * Author             : deGroot
 * Family             : YaskawaMce
 * Organisation       : github.com/YaskawaEurope/mlx-examples
 * 
 * -----------------------------------------------------------------------------
 * Performed a trajectory stored in a PosTable
 * -----------------------------------------------------------------------------
 *)
 
 posTable := posTable_In;
 io := io_In;

// =============================================================================
// 1. INITIALIZATION & EDGE DETECTION
// =============================================================================
// Calculate table boundaries safely
nPosTableSize := TO_INT(SIZEOF(posTable) / SIZEOF(posTable.stEntry[0]));
io.nIndex := LIMIT(0, io.nIndex, nPosTableSize - 1);
io.nLoadIndex := LIMIT(0, io.nLoadIndex, nPosTableSize - 1);

// Generate pulses (One-Shots) for inputs
bOsrStep := io.bStep AND NOT aOneShots[0];
aOneShots[0] := io.bStep;

bOsrSystemReady := io.bSystemReady AND NOT aOneShots[1];
aOneShots[1] := io.bSystemReady;

bOsrRecalcQA := io.bRecalcQA AND NOT aOneShots[2];
aOneShots[2] := io.bRecalcQA;

bOsfRun := NOT io.bRun AND aOneShots[3];
aOneShots[3] := io.bRun;

// Search for the last entry of PosTable (either bLastEntry or last not skipped entry)
nLastEntryIndex := 0;
FOR i := 0 TO nPosTableSize - 1 DO
  IF posTable.stEntry[i].bLastEntry OR NOT posTable.stEntry[i].bSkipEntry THEN
    nLastEntryIndex := i;
    IF posTable.stEntry[i].bLastEntry THEN
      EXIT;
    END_IF;
  END_IF;
END_FOR;

bLastEntryInProcess := posTable.stEntry[io.nIndex].bLastEntry OR (io.nIndex = nLastEntryIndex);
bOsfLastMoveInProcess := NOT bLastEntryInProcess AND aOneShots[4];
aOneShots[4] := bLastEntryInProcess;

bOsrRun := io.bRun AND NOT aOneShots[5];
aOneShots[5] := io.bRun;

// Count the number of cycles
IF bOsfLastMoveInProcess AND io.bBusy THEN
  io.nCycleNr := io.nCycleNr + 1;
END_IF;

bStopAfterLastEntry := ((io.nCycleNr + 1) >= io.nNrOfCycles) AND NOT (io.nNrOfCycles = 0);

// Check if next conditions are valid to update the Queue array (QA)
bUpdateQA := FALSE;
IF io.bRun AND (io.nMode = PT_MODE_0) AND (nQA < QA_MAX) THEN
  nNextValidLoadIndex := THIS.McePosTableFindEntry(io.nLoadIndex, 1, nPosTableSize, NOT(bStopAfterLastEntry));
  IF nNextValidLoadIndex >= 0 AND posTable.stEntry[io.nLoadIndex].nActionID = 0 THEN
    stConditionResult := THIS.McePosTableCheckConditions(nNextValidLoadIndex, nPosTableSize, io.aConditions);
    bUpdateQA := stConditionResult.bConditionsOk;
  END_IF;
END_IF;

bWaiting := FALSE;

bAllowQueuing := (io.nMode = PT_MODE_0) AND io.bRun;

Out := MEMORY_SET(stConditionResult, 0, BYTE#0, SIZEOF(stConditionResult));

// clear Enable signals but keep signals enabled if an error occurs
IF io.nSmPosTable <> 99 THEN
  Out := MEMORY_SET(aEnable, 0, BYTE#0, SIZEOF(aEnable));
END_IF;
io.bError := FALSE;

// Update QA
bUpdateQA := bUpdateQA OR bOsrRecalcQA OR (bOsrSystemReady AND io.nSmPosTable >= 10 AND io.nSmPosTable < 30);
IF bUpdateQA THEN
  bFirstMove := bFirstMove OR bOsrSystemReady OR bOsrRun;
  THIS.McePosTableRecalcQA(bAllowQueuing, bFirstMove, bStopAfterLastEntry, io.aConditions, io.nLoadIndex);
END_IF;

// -----------------------------------------------------------------------------
// state machine 1: Motion
// -----------------------------------------------------------------------------
CASE io.nSmPosTable OF
  // -------------------------------------
  // state 0 - idle, not ready for start
  // -------------------------------------
  0:
    IF bOsfRun THEN 
      io.nSmPosTable := 60;
    ELSIF io.bSystemReady THEN
      io.nErrorCode := 0;
      io.nSmPosTable := 1;
    END_IF;

  // -------------------------------------
  // state 1 - idle, ready for start
  // -------------------------------------
  1:
    IF NOT io.bSystemReady THEN 
      io.nSmPosTable := 0;
    ELSIF bOsfRun THEN 
      io.nSmPosTable := 60;
    ELSIF io.bSystemReady THEN
        
      bWaiting := (io.nMode = PT_MODE_2) AND io.bRun;
      bFirstMove := TRUE;

      IF ((io.nMode <> PT_MODE_2 AND io.bRun) OR (io.nMode = PT_MODE_2 AND bOsrStep AND io.bRun)) THEN
        io.nLoadIndex := io.nIndex;
        THIS.McePosTableUpdateIndex(bAllowQueuing, bFirstMove, bStopAfterLastEntry, io.aConditions, io.nLoadIndex);
        
        IF io.nIndex < 0 THEN
          io.nIndex := 0;
          io.nSmPosTable := 50;
        ELSE
          stConditionResult := THIS.McePosTableCheckConditions(io.nIndex, nPosTableSize, io.aConditions);
          IF NOT stConditionResult.bConditionsOk THEN 
            io.nSmPosTable := 5; 
          ELSE 
            io.nSmPosTable := 10; 
          END_IF;
        END_IF;
      END_IF;
    END_IF;

  // -------------------------------------
  // state 5 - wait for conditions
  // -------------------------------------
  5:
    IF NOT io.bRun THEN 
      io.nSmPosTable := 50;
    ELSIF io.bSystemReady THEN
      stConditionResult := THIS.McePosTableCheckConditions(io.nIndex, nPosTableSize, io.aConditions);
      
      IF stConditionResult.bConditionsOk AND ((io.nMode <> PT_MODE_2 AND io.bRun) OR (bOsrStep OR (nQA > 0))) THEN
        bWaitBeforeNextEntry := FALSE;
        THIS.McePosTableRecalcQA(bAllowQueuing, bFirstMove, bStopAfterLastEntry, io.aConditions, io.nLoadIndex);
        io.nSmPosTable := 10;
      END_IF;
    END_IF;

  // -------------------------------------
  // state 10..29 - handle motion commands
  // -------------------------------------
  10..29:
    FOR i := 0 TO QA_MAX DO
      nInstanceNumber[i] := io.nSmPosTable + TO_INT(i) - 10;
      IF nInstanceNumber[i] > QA_MAX THEN 
        nInstanceNumber[i] := nInstanceNumber[i] - (QA_MAX + 1); 
      END_IF;
    END_FOR;

    IF NOT io.bRun AND ((io.nAbortingMode = 1) OR NOT io.bSystemReady) THEN 
      io.nSmPosTable := 50;
    ELSIF io.bSystemReady THEN
      // start this motion
      aEnable[nInstanceNumber[0]] := TRUE;

      // queue next motions
      FOR i := 1 TO QA_MAX DO
        aEnable[nInstanceNumber[i]] := (aMoveCmd[nInstanceNumber[i-1]].bSts_EN AND aMoveCmd[nInstanceNumber[i-1]].bSts_DN) AND (nQA >= i);
      END_FOR;

      // motion completed
      IF (aMoveCmd[nInstanceNumber[0]].bSts_EN AND aMoveCmd[nInstanceNumber[0]].bSts_PC) THEN
        IF NOT io.bRun AND (nQA = 0) THEN 
          io.nSmPosTable := 50;
        ELSIF bStopEntry THEN 
          io.nSmPosTable := 30; // Need to stop and do an action
        ELSIF bLastEntry THEN 
          io.nSmPosTable := 40; // End of path
        ELSE
          bWaiting := (io.nMode = PT_MODE_2);
          IF (io.nMode <> PT_MODE_2 AND io.bRun) OR (bOsrStep OR (nQA > 0)) THEN
            aEnable[nInstanceNumber[0]] := FALSE;
            IF bWaitBeforeNextEntry THEN 
              io.nSmPosTable := 5; 
            ELSE 
              io.nSmPosTable := nInstanceNumber[1] + 10; 
            END_IF;
            THIS.McePosTableUpdateIndex(bAllowQueuing, bFirstMove, bStopAfterLastEntry, io.aConditions, io.nLoadIndex);
          END_IF;
        END_IF;
      END_IF;
    END_IF;

  // -------------------------------------
  // state 30 - waiting before performing action at standstill
  // -------------------------------------
  30:
    IF NOT io.bRun THEN 
      io.nSmPosTable := 50;
    ELSIF io.bSystemReady THEN
      bWaiting := (io.nMode = PT_MODE_2);
      IF (io.nMode <> PT_MODE_2) OR bOsrStep THEN 
        io.nSmPosTable := 31; 
      END_IF;
    END_IF;

  // -------------------------------------
  // state 31 - perform action at standstill
  // -------------------------------------
  31:
    IF NOT io.bRun THEN 
     io.nSmPosTable := 50;
    ELSIF io.bSystemReady THEN
      bWaiting := (bDefaultActionDone OR io.bCustomActionDone) AND (io.nMode = PT_MODE_2);
      bCustomActionDone := io.bCustomActionDone AND (io.nActionID > RESERVED_ACTIONID_UBOUND);
      IF bDefaultActionDone OR bCustomActionDone THEN 
        io.nSmPosTable := 32; 
      END_IF;
    END_IF;

  // -------------------------------------
  // state 32 - waiting after performing action
  // -------------------------------------
  32:
    IF NOT io.bRun THEN 
      io.nSmPosTable := 50;
    ELSIF io.bSystemReady THEN
      bWaiting := (io.nMode = PT_MODE_2);
      IF (io.nMode <> PT_MODE_2) OR bOsrStep THEN
        IF bLastEntry AND (io.nNrOfCycles - io.nCycleNr <= 1) THEN 
          io.nSmPosTable := 40;
        ELSE
          IF bWaitBeforeNextEntry THEN 
            io.nSmPosTable := 5; 
          ELSE 
            io.nSmPosTable := 10; 
          END_IF;
          THIS.McePosTableUpdateIndex(bAllowQueuing, bFirstMove, bStopAfterLastEntry, io.aConditions, io.nLoadIndex);
        END_IF;
      END_IF;
    END_IF;

  // -------------------------------------
  // state 40 - cycle completed
  // -------------------------------------
  40:
    io.nCycleNr := io.nCycleNr + 1;
    IF NOT io.bRun THEN 
      io.nSmPosTable := 50;
    ELSIF ((io.nCycleNr < io.nNrOfCycles) OR (io.nNrOfCycles = 0)) AND (io.nMode <> PT_MODE_2) THEN
      IF bWaitBeforeNextEntry THEN 
        io.nSmPosTable := 5; 
      ELSE 
        io.nSmPosTable := 10; 
      END_IF;
      THIS.McePosTableUpdateIndex(bAllowQueuing, bFirstMove, FALSE, io.aConditions, io.nLoadIndex);
    ELSE 
      io.nSmPosTable := 50;
    END_IF;

  // -------------------------------------
  // state 50 - wait at the end of cycle
  // -------------------------------------
  50: 
    IF NOT io.bRun THEN 
      io.nSmPosTable := 60; 
    END_IF;
    
  // -------------------------------------
  // state 60 - reset cycle
  // -------------------------------------
  60:
    io.nIndex := 0;
    io.nLoadIndex := 0;
    io.nCycleNr := 0;
    io.nSmPosTable := 0;

  // -------------------------------------
  // state 99 - state machine error
  // -------------------------------------
  99:
    io.bError := TRUE;
    IF NOT io.bRun THEN
      Out := MEMORY_SET(aEnable, 0, BYTE#0, SIZEOF(aEnable));
      io.nSmPosTable := 60;
    END_IF;
END_CASE;

// -----------------------------------------------------------------------------
// actions
// -----------------------------------------------------------------------------
io.bActionStart := (io.nSmPosTable = 31);
io.nActionID := posTable.stEntry[io.nIndex].nActionID;

IF io.bActionStart THEN
  // map PosTable actions
  CASE io.nActionID OF
    ACTIONID_NO_ACTION:
      bDefaultActionDone := TRUE;
    ACTIONID_WAIT_ONE_SCAN:
      bDefaultActionDone := TRUE;
    ACTIONID_WAIT_STEP:
      bWaiting := TRUE; 
      bDefaultActionDone := bOsrStep;
  ELSE 
    bDefaultActionDone := io.nActionID <= RESERVED_ACTIONID_UBOUND;
  END_CASE;
ELSE 
  bDefaultActionDone := FALSE; 
END_IF;

// -----------------------------------------------------------------------------
// state monitoring
// Waiting condition is used to exclude the time where it is waiting for the Step button
// -----------------------------------------------------------------------------
io.nStateTime := THIS.MceStateMonitoring(io.nSmPosTable, bWaiting OR NOT io.bSystemReady OR (io.nSmPosTable >= 40) OR (io.nSmPosTable < 10), stStateMonitoringData);

// -----------------------------------------------------------------------------
// process move commands
// -----------------------------------------------------------------------------
// Rising edge signals of Enable
FOR i := 0 TO QA_MAX DO
  aOsrEnable[i] := aEnable[i] AND NOT aOneShots[i+6];
  aOneShots[i+6] := aEnable[i];
  
  IF aOsrEnable[i] THEN
    THIS.McePosTableUpdateLoadIndex(bAllowQueuing, bFirstMove, ((io.nCycleNr + 1) >= io.nNrOfCycles) AND NOT (io.nNrOfCycles = 0), io.nIndex, io.aConditions);
    
    aUseMoveType[i] := posTable.stEntry[io.nLoadIndex].nMoveType;
    nUserFrameNr := SINT_TO_DINT(posTable.stEntry[io.nLoadIndex].nUserFrameNumber);
    nToolNr := SINT_TO_DINT(posTable.stEntry[io.nLoadIndex].nToolNumber);

    // ROUTE TO EXPLICIT FB INSTANCES
    CASE i OF
      // -----------------------------------------------------------------------
      // INSTANCE 0
      // -----------------------------------------------------------------------
      0:
        IF aUseMoveType[0] = 1 THEN
          // --- Linear Motion Setup ---
          fbRobotMoveLinearAbsolute_0.RobotNumber := io.nRobotNumber;
          fbRobotMoveLinearAbsolute_0.TargetType  := posTable.stEntry[io.nLoadIndex].bTargetType;
          
          Out := MEMORY_SET(fbRobotMoveLinearAbsolute_0.TargetPosition,0, BYTE#0,SIZEOF(fbRobotMoveLinearAbsolute_0.TargetPosition));
          
          IF posTable.stEntry[io.nLoadIndex].bTargetType THEN
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveLinearAbsolute_0.TargetPosition.TCPPosition.TCPPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          ELSE
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0,fbRobotMoveLinearAbsolute_0.TargetPosition.AxisPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          END_IF;

          fbRobotMoveLinearAbsolute_0.BlendFactor        := SINT_TO_DINT(posTable.stEntry[io.nLoadIndex].nBlendFactor);
          fbRobotMoveLinearAbsolute_0.Speed              := posTable.stEntry[io.nLoadIndex].fSpeed;
          fbRobotMoveLinearAbsolute_0.UseRotationalSpeed := posTable.stEntry[io.nLoadIndex].bUseRotationalSpeed;
          fbRobotMoveLinearAbsolute_0.SpeedUnits         := posTable.stEntry[io.nLoadIndex].nSpeedUnits;
          fbRobotMoveLinearAbsolute_0.Acceleration       := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nAcc);
          fbRobotMoveLinearAbsolute_0.Deceleration       := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nDec);
        ELSE
          // --- PTP Axis Motion Setup ---
          fbRobotMoveAxisAbsolute_0.RobotNumber := io.nRobotNumber;
          fbRobotMoveAxisAbsolute_0.TargetType  := posTable.stEntry[io.nLoadIndex].bTargetType;
          
          Out := MEMORY_SET(fbRobotMoveAxisAbsolute_0.TargetPosition, 0, BYTE#0,SIZEOF(fbRobotMoveAxisAbsolute_0.TargetPosition));
          
          IF posTable.stEntry[io.nLoadIndex].bTargetType THEN
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0,fbRobotMoveAxisAbsolute_0.TargetPosition.TCPPosition.TCPPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          ELSE
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveAxisAbsolute_0.TargetPosition.AxisPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          END_IF;

          fbRobotMoveAxisAbsolute_0.BlendFactor  := SINT_TO_DINT(posTable.stEntry[io.nLoadIndex].nBlendFactor);
          fbRobotMoveAxisAbsolute_0.Speed        := posTable.stEntry[io.nLoadIndex].fSpeed;
          fbRobotMoveAxisAbsolute_0.Acceleration := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nAcc);
          fbRobotMoveAxisAbsolute_0.Deceleration := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nDec);
        END_IF;

        // --- Tool & Frame Setup ---
        fbRobotSetUserFrame_0.RobotNumber          := io.nRobotNumber;
        fbRobotSetUserFrame_0.UserFrameNumber      := nUserFrameNr;
        fbRobotSetUserFrame_0.UserFrameData.CoordFrame := userFrames.aFrames[nUserFrameNr].aVector;
        
        fbRobotSelectTool_0.RobotNumber := io.nRobotNumber;
        fbRobotSelectTool_0.ToolNumber  := nToolNr;
        fbRobotSelectTool_0.ToolData    := tools.aTools[nToolNr];

      // -----------------------------------------------------------------------
      // INSTANCE 1
      // -----------------------------------------------------------------------
      1:
        IF aUseMoveType[1] = 1 THEN
          fbRobotMoveLinearAbsolute_1.RobotNumber := io.nRobotNumber;
          fbRobotMoveLinearAbsolute_1.TargetType  := posTable.stEntry[io.nLoadIndex].bTargetType;
          Out := MEMORY_SET(fbRobotMoveLinearAbsolute_1.TargetPosition, 0, BYTE#0, SIZEOF(fbRobotMoveLinearAbsolute_1.TargetPosition));
          IF posTable.stEntry[io.nLoadIndex].bTargetType THEN
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveLinearAbsolute_1.TargetPosition.TCPPosition.TCPPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          ELSE
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveLinearAbsolute_1.TargetPosition.AxisPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          END_IF;
          fbRobotMoveLinearAbsolute_1.BlendFactor        := SINT_TO_DINT(posTable.stEntry[io.nLoadIndex].nBlendFactor);
          fbRobotMoveLinearAbsolute_1.Speed              := posTable.stEntry[io.nLoadIndex].fSpeed;
          fbRobotMoveLinearAbsolute_1.UseRotationalSpeed := posTable.stEntry[io.nLoadIndex].bUseRotationalSpeed;
          fbRobotMoveLinearAbsolute_1.SpeedUnits         := posTable.stEntry[io.nLoadIndex].nSpeedUnits;
          fbRobotMoveLinearAbsolute_1.Acceleration       := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nAcc);
          fbRobotMoveLinearAbsolute_1.Deceleration       := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nDec);
        ELSE
          fbRobotMoveAxisAbsolute_1.RobotNumber := io.nRobotNumber;
          fbRobotMoveAxisAbsolute_1.TargetType  := posTable.stEntry[io.nLoadIndex].bTargetType;
          Out := MEMORY_SET(fbRobotMoveAxisAbsolute_1.TargetPosition, 0, BYTE#0, SIZEOF(fbRobotMoveAxisAbsolute_1.TargetPosition));
          IF posTable.stEntry[io.nLoadIndex].bTargetType THEN
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveAxisAbsolute_1.TargetPosition.TCPPosition.TCPPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          ELSE
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveAxisAbsolute_1.TargetPosition.AxisPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          END_IF;
          fbRobotMoveAxisAbsolute_1.BlendFactor  := SINT_TO_DINT(posTable.stEntry[io.nLoadIndex].nBlendFactor);
          fbRobotMoveAxisAbsolute_1.Speed        := posTable.stEntry[io.nLoadIndex].fSpeed;
          fbRobotMoveAxisAbsolute_1.Acceleration := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nAcc);
          fbRobotMoveAxisAbsolute_1.Deceleration := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nDec);
        END_IF;

        fbRobotSetUserFrame_1.RobotNumber          := io.nRobotNumber;
        fbRobotSetUserFrame_1.UserFrameNumber      := nUserFrameNr;
        fbRobotSetUserFrame_1.UserFrameData.CoordFrame := userFrames.aFrames[nUserFrameNr].aVector;
        
        fbRobotSelectTool_1.RobotNumber := io.nRobotNumber;
        fbRobotSelectTool_1.ToolNumber  := nToolNr;
        fbRobotSelectTool_1.ToolData    := tools.aTools[nToolNr];

      // -----------------------------------------------------------------------
      // INSTANCE 2
      // -----------------------------------------------------------------------
      2:
        IF aUseMoveType[2] = 1 THEN
          fbRobotMoveLinearAbsolute_2.RobotNumber := io.nRobotNumber;
          fbRobotMoveLinearAbsolute_2.TargetType  := posTable.stEntry[io.nLoadIndex].bTargetType;
          Out := MEMORY_SET(fbRobotMoveLinearAbsolute_2.TargetPosition, 0, BYTE#0, SIZEOF(fbRobotMoveLinearAbsolute_2.TargetPosition));
          IF posTable.stEntry[io.nLoadIndex].bTargetType THEN
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveLinearAbsolute_2.TargetPosition.TCPPosition.TCPPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          ELSE
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveLinearAbsolute_2.TargetPosition.AxisPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          END_IF;
          fbRobotMoveLinearAbsolute_2.BlendFactor        := SINT_TO_DINT(posTable.stEntry[io.nLoadIndex].nBlendFactor);
          fbRobotMoveLinearAbsolute_2.Speed              := posTable.stEntry[io.nLoadIndex].fSpeed;
          fbRobotMoveLinearAbsolute_2.UseRotationalSpeed := posTable.stEntry[io.nLoadIndex].bUseRotationalSpeed;
          fbRobotMoveLinearAbsolute_2.SpeedUnits         := posTable.stEntry[io.nLoadIndex].nSpeedUnits;
          fbRobotMoveLinearAbsolute_2.Acceleration       := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nAcc);
          fbRobotMoveLinearAbsolute_2.Deceleration       := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nDec);
        ELSE
          fbRobotMoveAxisAbsolute_2.RobotNumber := io.nRobotNumber;
          fbRobotMoveAxisAbsolute_2.TargetType  := posTable.stEntry[io.nLoadIndex].bTargetType;
          Out := MEMORY_SET(fbRobotMoveAxisAbsolute_2.TargetPosition, 0, BYTE#0, SIZEOF(fbRobotMoveAxisAbsolute_2.TargetPosition));
          IF posTable.stEntry[io.nLoadIndex].bTargetType THEN
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0,  fbRobotMoveAxisAbsolute_2.TargetPosition.TCPPosition.TCPPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          ELSE
            Out := MEMORY_COPY(posTable.stEntry[io.nLoadIndex].aPosition, 0, fbRobotMoveAxisAbsolute_2.TargetPosition.AxisPosition, 0, SIZEOF(posTable.stEntry[io.nLoadIndex].aPosition));
          END_IF;
          fbRobotMoveAxisAbsolute_2.BlendFactor  := SINT_TO_DINT(posTable.stEntry[io.nLoadIndex].nBlendFactor);
          fbRobotMoveAxisAbsolute_2.Speed        := posTable.stEntry[io.nLoadIndex].fSpeed;
          fbRobotMoveAxisAbsolute_2.Acceleration := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nAcc);
          fbRobotMoveAxisAbsolute_2.Deceleration := SINT_TO_REAL(posTable.stEntry[io.nLoadIndex].nDec);
        END_IF;

        fbRobotSetUserFrame_2.RobotNumber          := io.nRobotNumber;
        fbRobotSetUserFrame_2.UserFrameNumber      := nUserFrameNr;
        fbRobotSetUserFrame_2.UserFrameData.CoordFrame := userFrames.aFrames[nUserFrameNr].aVector;
        
        fbRobotSelectTool_2.RobotNumber := io.nRobotNumber;
        fbRobotSelectTool_2.ToolNumber  := nToolNr;
        fbRobotSelectTool_2.ToolData    := tools.aTools[nToolNr];
        
    END_CASE;

    // change user frame; if the user frame number differs from the previous stEntry a MLxRobotSetUserFrame will be triggered just before the move
    aChangeUserFrame[i] := bFirstMove OR (nUserFrameNr <> INT_TO_DINT(MLX.Robot[io.nRobotNumber].ActiveUserFrameNumber));
    // change (select) tool; if the tool number differs from the previous stEntry a MLxRobotSelectTool will be triggered just before the move
    aChangeTool[i] := bFirstMove OR (nToolNr <> INT_TO_DINT(MLX.Robot[io.nRobotNumber].ActiveToolNumber));
    bFirstMove := FALSE;
  END_IF;

  // ---------------------------------------------------------------------------
  // ENABLE LOGIC & FB EXECUTION
  // ---------------------------------------------------------------------------
  CASE i OF
    0:
      bUserFrameDone := NOT aChangeUserFrame[0] OR (fbRobotSetUserFrame_0.Sts_EN AND fbRobotSetUserFrame_0.Sts_DN AND NOT fbRobotSetUserFrame_0.Sts_ER);
      bToolDone      := NOT aChangeTool[0]      OR (fbRobotSelectTool_0.Sts_EN   AND fbRobotSelectTool_0.Sts_DN   AND NOT fbRobotSelectTool_0.Sts_ER);
      
      fbRobotMoveAxisAbsolute_0.Enable   := aEnable[0] AND NOT (aUseMoveType[0] = 1) AND bUserFrameDone AND bToolDone;
      fbRobotMoveLinearAbsolute_0.Enable := aEnable[0] AND (aUseMoveType[0] = 1)     AND bUserFrameDone AND bToolDone;
      fbRobotSetUserFrame_0.Enable       := aEnable[0] AND aChangeUserFrame[0];
      fbRobotSelectTool_0.Enable         := aEnable[0] AND aChangeTool[0];

      // Execute Functions
      fbRobotSelectTool_0(MLX := MLX);
      fbRobotSetUserFrame_0(MLX := MLX);
      fbRobotMoveAxisAbsolute_0(MLX := MLX);
      fbRobotMoveLinearAbsolute_0(MLX := MLX);

      // Read Status
      IF aUseMoveType[0] = 1 THEN
        aMoveCmd[0].bSts_EN := fbRobotMoveLinearAbsolute_0.Sts_EN;
        aMoveCmd[0].bSts_DN := fbRobotMoveLinearAbsolute_0.Sts_DN;
        aMoveCmd[0].bSts_IP := fbRobotMoveLinearAbsolute_0.Sts_IP;
        aMoveCmd[0].bSts_AC := fbRobotMoveLinearAbsolute_0.Sts_AC;
        aMoveCmd[0].bSts_PC := fbRobotMoveLinearAbsolute_0.Sts_PC;
        aMoveCmd[0].bSts_ER := fbRobotMoveLinearAbsolute_0.Sts_ER;
        aMoveCmd[0].nPercentComplete := fbRobotMoveLinearAbsolute_0.PercentComplete;
      ELSE
        aMoveCmd[0].bSts_EN := fbRobotMoveAxisAbsolute_0.Sts_EN;
        aMoveCmd[0].bSts_DN := fbRobotMoveAxisAbsolute_0.Sts_DN;
        aMoveCmd[0].bSts_IP := fbRobotMoveAxisAbsolute_0.Sts_IP;
        aMoveCmd[0].bSts_AC := fbRobotMoveAxisAbsolute_0.Sts_AC;
        aMoveCmd[0].bSts_PC := fbRobotMoveAxisAbsolute_0.Sts_PC;
        aMoveCmd[0].bSts_ER := fbRobotMoveAxisAbsolute_0.Sts_ER;
        aMoveCmd[0].nPercentComplete := fbRobotMoveAxisAbsolute_0.PercentComplete;
      END_IF;

    1:
      bUserFrameDone := NOT aChangeUserFrame[1] OR (fbRobotSetUserFrame_1.Sts_EN AND fbRobotSetUserFrame_1.Sts_DN AND NOT fbRobotSetUserFrame_1.Sts_ER);
      bToolDone      := NOT aChangeTool[1]      OR (fbRobotSelectTool_1.Sts_EN   AND fbRobotSelectTool_1.Sts_DN   AND NOT fbRobotSelectTool_1.Sts_ER);
      
      fbRobotMoveAxisAbsolute_1.Enable   := aEnable[1] AND NOT (aUseMoveType[1] = 1) AND bUserFrameDone AND bToolDone;
      fbRobotMoveLinearAbsolute_1.Enable := aEnable[1] AND (aUseMoveType[1] = 1)     AND bUserFrameDone AND bToolDone;
      fbRobotSetUserFrame_1.Enable       := aEnable[1] AND aChangeUserFrame[1];
      fbRobotSelectTool_1.Enable         := aEnable[1] AND aChangeTool[1];

      fbRobotSelectTool_1(MLX := MLX);
      fbRobotSetUserFrame_1(MLX := MLX);
      fbRobotMoveAxisAbsolute_1(MLX := MLX);
      fbRobotMoveLinearAbsolute_1(MLX := MLX);

      IF aUseMoveType[1] = 1 THEN
        aMoveCmd[1].bSts_EN := fbRobotMoveLinearAbsolute_1.Sts_EN;
        aMoveCmd[1].bSts_DN := fbRobotMoveLinearAbsolute_1.Sts_DN;
        aMoveCmd[1].bSts_IP := fbRobotMoveLinearAbsolute_1.Sts_IP;
        aMoveCmd[1].bSts_AC := fbRobotMoveLinearAbsolute_1.Sts_AC;
        aMoveCmd[1].bSts_PC := fbRobotMoveLinearAbsolute_1.Sts_PC;
        aMoveCmd[1].bSts_ER := fbRobotMoveLinearAbsolute_1.Sts_ER;
        aMoveCmd[1].nPercentComplete := fbRobotMoveLinearAbsolute_1.PercentComplete;
      ELSE
        aMoveCmd[1].bSts_EN := fbRobotMoveAxisAbsolute_1.Sts_EN;
        aMoveCmd[1].bSts_DN := fbRobotMoveAxisAbsolute_1.Sts_DN;
        aMoveCmd[1].bSts_IP := fbRobotMoveAxisAbsolute_1.Sts_IP;
        aMoveCmd[1].bSts_AC := fbRobotMoveAxisAbsolute_1.Sts_AC;
        aMoveCmd[1].bSts_PC := fbRobotMoveAxisAbsolute_1.Sts_PC;
        aMoveCmd[1].bSts_ER := fbRobotMoveAxisAbsolute_1.Sts_ER;
        aMoveCmd[1].nPercentComplete := fbRobotMoveAxisAbsolute_1.PercentComplete;
      END_IF;

    2:
      bUserFrameDone := NOT aChangeUserFrame[2] OR (fbRobotSetUserFrame_2.Sts_EN AND fbRobotSetUserFrame_2.Sts_DN AND NOT fbRobotSetUserFrame_2.Sts_ER);
      bToolDone      := NOT aChangeTool[2]      OR (fbRobotSelectTool_2.Sts_EN   AND fbRobotSelectTool_2.Sts_DN   AND NOT fbRobotSelectTool_2.Sts_ER);
      
      fbRobotMoveAxisAbsolute_2.Enable   := aEnable[2] AND NOT (aUseMoveType[2] = 1) AND bUserFrameDone AND bToolDone;
      fbRobotMoveLinearAbsolute_2.Enable := aEnable[2] AND (aUseMoveType[2] = 1)     AND bUserFrameDone AND bToolDone;
      fbRobotSetUserFrame_2.Enable       := aEnable[2] AND aChangeUserFrame[2];
      fbRobotSelectTool_2.Enable         := aEnable[2] AND aChangeTool[2];

      fbRobotSelectTool_2(MLX := MLX);
      fbRobotSetUserFrame_2(MLX := MLX);
      fbRobotMoveAxisAbsolute_2(MLX := MLX);
      fbRobotMoveLinearAbsolute_2(MLX := MLX);

      IF aUseMoveType[2] = 1 THEN
        aMoveCmd[2].bSts_EN := fbRobotMoveLinearAbsolute_2.Sts_EN;
        aMoveCmd[2].bSts_DN := fbRobotMoveLinearAbsolute_2.Sts_DN;
        aMoveCmd[2].bSts_IP := fbRobotMoveLinearAbsolute_2.Sts_IP;
        aMoveCmd[2].bSts_AC := fbRobotMoveLinearAbsolute_2.Sts_AC;
        aMoveCmd[2].bSts_PC := fbRobotMoveLinearAbsolute_2.Sts_PC;
        aMoveCmd[2].bSts_ER := fbRobotMoveLinearAbsolute_2.Sts_ER;
        aMoveCmd[2].nPercentComplete := fbRobotMoveLinearAbsolute_2.PercentComplete;
      ELSE
        aMoveCmd[2].bSts_EN := fbRobotMoveAxisAbsolute_2.Sts_EN;
        aMoveCmd[2].bSts_DN := fbRobotMoveAxisAbsolute_2.Sts_DN;
        aMoveCmd[2].bSts_IP := fbRobotMoveAxisAbsolute_2.Sts_IP;
        aMoveCmd[2].bSts_AC := fbRobotMoveAxisAbsolute_2.Sts_AC;
        aMoveCmd[2].bSts_PC := fbRobotMoveAxisAbsolute_2.Sts_PC;
        aMoveCmd[2].bSts_ER := fbRobotMoveAxisAbsolute_2.Sts_ER;
        aMoveCmd[2].nPercentComplete := fbRobotMoveAxisAbsolute_2.PercentComplete;
      END_IF;

  END_CASE;
END_FOR;

// -----------------------------------------------------------------------------
// map nPercentComplete
// -----------------------------------------------------------------------------
CASE io.nSmPosTable OF
  10..29: 
    io.nPercentComplete := aMoveCmd[nInstanceNumber[0]].nPercentComplete;
  30..50: 
    io.nPercentComplete := 100;
  ELSE    
    io.nPercentComplete := 0;
END_CASE;

// -----------------------------------------------------------------------------
// PosTable status
// -----------------------------------------------------------------------------
io.bWaitForStep := bWaiting;
io.bDone := (io.nSmPosTable = 50);
io.bIdle := (io.nSmPosTable < 5);
io.bBusy := (io.nSmPosTable >= 5 AND io.nSmPosTable < 50);
io.aMissingConditions := stConditionResult.aMissingConditions;

// Catch global errors from the explicit instances
bError := MLX.JoggingMode;

// Check errors for Instance 0
bError := bError OR (aMoveCmd[0].bSts_EN AND aMoveCmd[0].bSts_ER) OR 
          (fbRobotSelectTool_0.Sts_EN AND fbRobotSelectTool_0.Sts_ER) OR 
          (fbRobotSetUserFrame_0.Sts_EN AND fbRobotSetUserFrame_0.Sts_ER);

// Check errors for Instance 1
bError := bError OR (aMoveCmd[1].bSts_EN AND aMoveCmd[1].bSts_ER) OR 
          (fbRobotSelectTool_1.Sts_EN AND fbRobotSelectTool_1.Sts_ER) OR 
          (fbRobotSetUserFrame_1.Sts_EN AND fbRobotSetUserFrame_1.Sts_ER);

// Check errors for Instance 2
bError := bError OR (aMoveCmd[2].bSts_EN AND aMoveCmd[2].bSts_ER) OR 
          (fbRobotSelectTool_2.Sts_EN AND fbRobotSelectTool_2.Sts_ER) OR 
          (fbRobotSetUserFrame_2.Sts_EN AND fbRobotSetUserFrame_2.Sts_ER);

IF bError AND NOT (io.nSmPosTable = 99) THEN
  io.nErrorCode := 1000 + INT_TO_UDINT(io.nSmPosTable);
  io.nSmPosTable := 99;
END_IF;

posTable_In := posTable;
io_In := io;