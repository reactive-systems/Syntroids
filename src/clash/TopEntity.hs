module TopEntity where

import Clash.Prelude
import Functions
import Gamemodule
import EnemeyModule
import Functions
import Scoreboard
import Cockpitboard
import Radarboard
import Gamelogic
import RotationCalculator
import GamemodeChooser
import ActionConverter
import LedMatrix
import SPIWriteManag
import SPIWriteClk
import SPIWriteSdi
import SPIReadManag
import SPIReadSdi
import SPIReadClk
import SPI
import SensorSelector
import SensorRegister
import RegManager
import Sensor
import SensorSubmodulChooser
import SensorPart
import SensorInit

{-# ANN topEntity (
    Synthesize
    { t_name    = "TopEntity"
        , t_inputs  = [ PortProduct "" [ PortName "CLOCK", PortName "RESET"], PortName "SDO" ]
        , t_output =
            PortProduct ""
          [ PortName "LED"
          , PortName "CLK_OUT"
          , PortName "C1"
          , PortName "C2"
          , PortName "DATA"
          , PortName "LAT"
          , PortName "OE"
          , PortName "CS_AG"
          , PortName "CS_M"
          , PortName "CS_ALT"
          , PortName "SDI"
          , PortName "SCK"
          , PortName "INT"
          , PortName "DRDY_M"
          , PortName "PM1_0"
          , PortName "PM1_1"
          , PortName "PM1_2"
          , PortName "PM1_3"
          , PortName "PM1_4"
          , PortName "PM1_5"
          , PortName "PM1_6"
          , PortName "PM1_7"
          ]
    }
  )
  #-}

topEntity :: SystemClockReset => Signal System Bit -> (
        Signal System (BitVector 7), 
        Signal System Bit, Signal System (BitVector 3), Signal System (BitVector 3) , Signal System (Unsigned 4), Signal System Bit, Signal System Bit,
        Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit,
        Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit, Signal System Bit
    )


topEntity sdo = (
    leds,  
    extclock, color1, color2, cordy, bufferPin, driverPin,
    csAccGyr, csMag, csAlt, sdi, spc, int, drdy_m,
    pm1_0, pm1_1, pm1_2, pm1_3, pm1_4, pm1_5, pm1_6, pm1_7
    )
    where
        -------- Some default values ----------
        leds    = pure 0x0
        int     = pure low
        drdy_m  = pure low
       
        pm1_0   = pure low
        pm1_1   = pure low
        pm1_2   = pure low
        pm1_3   = pure low
        pm1_4   = pure low
        pm1_5   = pure low
        pm1_6   = pure low
        pm1_7   = pure low

        ----------------------------------------------------------------------------------------------------
        ----------------------------------------- The Game -------------------------------------------------
        ----------------------------------------------------------------------------------------------------

        csAlt = pure high
        (sdi, csAccGyr, csMag, spc) = unbundle sensorPins  
        (sensorPins, accx :> accy :> accz :> gyrox :> gyroy :> gyroz :> magx :> magy :> magz :> Nil) = sensorWrapper sdo              
                                                    ---- DO WE STILL NEED THIS ??????
        (gamestart, shot)   = actionConverterWrapper gamemode (register 0 accz) (gyrox, gyroy, gyroz)
        rotation            = rotationCalculatorWrapper (gyrox, gyroz) gamemode
        gamemode            = gamemodeChooserWrapper gyroy
         
        (score, gameover, enemies, scorecolor) = gameLogicManagerWrapper (gamestart, rotation, shot)
        scoreboardpoint     = scoreBoardWrapper (scorecolor, score, gameover)
        radarboardpoint     = radarBoardWrapper (enemies, rotation)
        cockpitboardpoint   = cockpitBoardWrapper (enemies, rotation)
        outpoint            = gamemoduleWrapper gamemode gameover cockpitboardpoint radarboardpoint scoreboardpoint
        (xcoordinate, ycoordinate, writecolor)   = unbundle $ outpoint 
        (_, color1, color2, extclock, cordy, bufferPin, driverPin) = ledMatrixWrapper xcoordinate ycoordinate writecolor write

        ----------------------------------------- Initial Blocker -------------------------------------------
        initalCounter = register (0 :: Unsigned 27) ((\c -> if c < 2^26 then c + 1 else c) <$> initalCounter)
        write = (\c -> c == 2^26) <$> initalCounter
        ------------------------------------------------------------------------------------------------------              

{-# NOINLINE topEntity #-}

----------------------------------------  Synthesized - Modules ----------------------------------------

gamemoduleWrapper :: (HiddenClockReset dom gated sync) => (Signal dom Gamemode) -> (Signal dom Bool) -> (Signal dom LEDPoint) -> (Signal dom LEDPoint) -> (Signal dom LEDPoint) -> (Signal dom LEDPoint)
gamemoduleWrapper gamemode gameover cockpitboardpoint radarboardpoint scoreboardpoint = 
    gamemodule iscockpitmode isradarmode isscoremode (0, 0, 0) cockpitboardpoint gamemode radarboardpoint scoreboardpoint gameover 

enemyModulWrapper :: (HiddenClockReset dom gated sync) => (Signal dom Bool, Signal dom Bool, Signal dom Angle, Signal dom Color) -> Signal dom (Angle, Radius, Color)
enemyModulWrapper (clock, reset, resetangle, incolor) = bundle (angle, radius, color)
    where
        -------------- Buffering due to the logic-loop effect ----------------
        radius = register 0xFF radius_out
        angle = register 0x10 angle_out
        ----------------------------------------------------------------------
        (angle_out, color, radius_out) = enemeyModule startradius (\z->z-1) (0 :: Angle) (0 :: Color) (0xFF :: Radius) incolor resetangle clock reset

gameLogicManagerWrapper :: (HiddenClockReset dom gated sync) => (Signal dom Bool, Signal dom Angle, Signal dom Bool) 
                                                   -> (Signal dom Score, Signal dom Bool, SignalEnemies dom, Signal dom Color)
gameLogicManagerWrapper (gamestart, rotation, shot) = (score, gameover, enemies, scorecolor)
    where
        (_, gameover, _, moveticks, _, resets, score, scorecolor, _) =  
            gamelogic (>0) hitenemy isgameover False (enemycount :: Unsigned 3) ticknomove True triggerresetnone 0 0 (\z->z-1)
                    fastestEnemy getenemyangle getenemyradius (+1) incmod incmoveclock newrand tickmove triggerreset triggerresetall 
                    (0 :: Unsigned 3) 
                    False 
                    ((0,0) :: (Unsigned 11, Unsigned 13)) 
                    (False :> False :> False :> False :> Nil)
                    (42 :: Angle)
                    ((False, 0 :: Angle) :> (False, 0) :> (False, 0) :> (False, 0) :> Nil)
                    (0 :: Score)
                    0b000
                    (0 :: Unsigned 3) 
                    (bundle enemies) rotation gamestart shot
        
        --------------------- Enemy bundling -----------------------------------------------
        t1 :> t2 :> t3 :> t4 :> Nil = unbundle moveticks
        (r1, a1) :> (r2, a2) :> (r3, a3) :> (r4, a4) :> Nil = map unbundle $ unbundle resets
        e1      = enemyModulWrapper (t1, r1, a1, pure 0b101) 
        e2      = enemyModulWrapper (t2, r2, a2, pure 0b010) 
        e3      = enemyModulWrapper (t3, r3, a3, pure 0b110) 
        e4      = enemyModulWrapper (t4, r4, a4, pure 0b001) 
        enemies = e1:>e2:>e3:>e4:>Nil
        ------------------------------------------------------------------------------------

scoreBoardWrapper :: (HiddenClockReset dom gated sync) => (Signal dom Color, Signal dom Score, Signal dom Bool) -> (Signal dom LEDPoint)
scoreBoardWrapper (scorecolor, score, gameover) = bundle (bxcoord, bycoord, actcolor)
    where
        (actcolor, bxcoord, bycoord, rampos, _, _)  = scoreboard (bitToBool . (! 0)) (==) blackcolor redcolor (0 :: Unsigned 5) (+) (\z->z-1) getRamPos (+1) 0 0 0 0 0 0 ramout score scorecolor gameover
        ramout = blockRamFile d5632 "ScoreDigits.txt" rampos (pure (Nothing :: Maybe (Unsigned 13, BitVector 1)))
                

cockpitBoardWrapper :: (HiddenClockReset dom gated sync) => (SignalEnemies dom, Signal dom Angle) -> (Signal dom LEDPoint)
cockpitBoardWrapper (enmies, rotation) = bundle(bxcoord, bycoord, color)
    where
        (bxcoord, bycoord, color, _, _, _, _, _) = 
            cockpitboard (==) (==0) hitcoord (<) blackcolor (sizex - 1) enemycount hairlinex hairliney
                         startradius whitecolor (\z->z-1) getenemyangle getenemycolor getenemyradius (+1) (+1) incmod 
                         (0 :: Unsigned 5) (0 :: Unsigned 5) (0 :: Color) (0 :: Unsigned 3) (0 :: Color) (255 :: Radius) (0 :: Unsigned 5) (0 :: Unsigned 5) 
                         (bundle enmies) rotation

radarBoardWrapper :: (HiddenClockReset dom gated sync) => (SignalEnemies dom, Signal dom Angle) -> (Signal dom LEDPoint)
radarBoardWrapper (enemies, rotation) = bundle (outx, outy, color)
    where
        (_, _, color, _, outx, outy, ramreqcosine, ramreqsine, _, _, _) =            
                    radarboard (==) ismiddel blackcolor enemycount sizex whitecolor cartesianx cartesiany (\z->z-1) getenemyangle getenemycolor getenemyradius (+1) (+1) incmod (-) 
                    (0 :: Color) (0xFF :: Radius) (0 :: Color) (0 :: Unsigned 5) (0 :: Unsigned 5) (0 :: Unsigned 5) (0 :: Angle) (0 :: Angle) 
                    (0 :: Color) (0 :: Unsigned 5) (0 :: Unsigned 5)
                    (bundle enemies) ramcosineout ramsineout rotation        
       
        ------------------------ Handmade lookup-tables ----------------------------------------------------- 
        ramsineout   = sinus_lookup $ (\a -> a +  64) <$> ramreqsine
        ramcosineout = sinus_lookup $ (\a -> a + 128) <$> ramreqcosine   
        sinus_lookup :: (HiddenClockReset dom gated sync) => (Signal dom Angle) -> (Signal dom (SFixed 10 5)) 
        sinus_lookup addr = unpack <$> (blockRamFile d256 "SinusTableInit.txt"  addr (pure Nothing)) 
        ------------------------------------------------------------------------------------------------------


rotationCalculatorWrapper :: (HiddenClockReset dom gated sync) => (Signal dom (Signed 16), Signal dom (Signed 16)) -> (Signal dom Gamemode) -> (Signal dom Angle)
rotationCalculatorWrapper (gyrox, gyroz) gamemode = rotation
    where
        (_, _, rotation) =   rotationCalculator (==0) gyronoise iscockpitmode isradarmode isscoremode 
                                                gyrocorrection roationcaluclatorprescaler addrotation incmod scalerotation subrotation 
                                                (0 :: Signed 28) (0 :: Unsigned 10) (0 :: Angle)
                                                gamemode gyrox gyroz 

gamemodeChooserWrapper :: (HiddenClockReset dom gated sync) => (Signal dom (Signed 16)) -> Signal dom Gamemode
gamemodeChooserWrapper gyroy = gamemode
    where
        (gamemode, _, _, _) = gamemodeChooser (==0) (==0) gamemodenoise (>) gyronoise iscockpitmode isradarmode isscoremode (<) Cockpit_Mode gamemodeswitch gyrocorrection 
                              Radar_Mode Score_Mode 0 addrotation (+1) (+1) (\c -> -c) Cockpit_Mode (0 :: Unsigned 10) (0 :: Unsigned 24) (0 :: Signed 28) gyroy

actionConverterWrapper :: (HiddenClockReset dom gated sync) => 
                            (Signal dom Gamemode) -> (Signal dom (Signed 16)) -> (Signal dom (Signed 16), Signal dom (Signed 16), Signal dom (Signed 16)) 
                            -> (Signal dom Bool, Signal dom Bool)
actionConverterWrapper gamemode accz (gyrox, gyroy, gyroz) = (gamestart, shot)
    where
       (gamestart, shot) = actionConverter (>) iscockpitmode isscoremode norotation False resetthreshhold shotthreshhold True abs False False accz gamemode gyrox gyroy gyroz


ledMatrixWrapper :: (HiddenClockReset dom gated sync) => Signal dom (Unsigned 5) -> Signal dom (Unsigned 5) -> Signal dom (BitVector 3) -> Signal dom Bool
                                                      -> (Signal dom (BitVector 3), Signal dom (BitVector 3), Signal dom (BitVector 3),
                                                          Signal dom Bit, Signal dom (Unsigned 4), Signal dom Bit, Signal dom Bit)
ledMatrixWrapper xcoordinate ycoordinate writecolor write = (color, color1, color2, extclock, cordy, bufferPin, driverPin)
    where 
        (bufferPin, color, color1, color2, _, cordy, driverPin, extclock, rampos, ramwrite, _) 
            = LedMatrix.ledMatrix   (==) (==0) high low sizex writeramnone (\z->z-1) (+1) (+1) (+1) rampos1 rampos2 ramposR writeram 
                                        high 0 0 0 (31 :: Unsigned 5) (0 :: Unsigned 4) high low (0 :: Unsigned 10) Nothing (0 :: Unsigned 7)
                                        ramout writecolor xcoordinate ycoordinate write
        
        --------------------------------- "Handmade" video memory ---------------------------------
        ramout     = blockRamFile d1024  "LEDMatrixInitial.txt" rampos ramwrite
        -------------------------------------------------------------------------------------------

sensorWrapper :: (HiddenClockReset dom gated sync) => (Signal dom SensorInput) -> (Signal dom SensorRealOutput, Vec 9 (Signal dom (Signed 16)))
sensorWrapper sdo = (sensorPins, registerVector)
    where
        sensorPins = sensorSelectorWrapper (spiPins, sensorType)
        (spiPins, spiResponse)     = spiWrapper sdo spiControl        
        registerVector = registerManagerWrapper regManagerCmd

        (regManagerCmd, sensorType, spiControl) = sensorSpecWrapper spiResponse 
        

sensorSpecWrapper :: (HiddenClockReset dom gated sync) => (Signal dom SpiOut) -> (Signal dom ManagerCtr, Signal dom SensorType, Signal dom SpiControl)
sensorSpecWrapper spiResponse = (regManagerCmd, sensorType, spiControl)
    where
        (magFinishedOut, regManagerCmdMag, sensorTypeMag, spiControlMag) = sensorPart sensorFinished False 5 4 MAG MAGNETOM NO_COMMAND 1 NONE 0x68 0x69 0x6A 0x6B 0x6C 0x6D 3 True 2 0 READ setRegister False NO_COMMAND NO_SENSOR NONE spiResponse startMag
        (accFinishedOut, regManagerCmdAcc, sensorTypeAcc, spiControlAcc) = sensorPart sensorFinished False 5 4 ACC ACC_GYRO NO_COMMAND 1 NONE 0x28 0x29 0x2A 0x2B 0x2C 0x2D 3 True 2 0 READ setRegister False NO_COMMAND NO_SENSOR NONE spiResponse startAcc
        (gyrFinishedOut, regManagerCmdGyr, sensorTypeGyr, spiControlGyr) = sensorPart sensorFinished False 5 4 GYRO ACC_GYRO NO_COMMAND 1 NONE 0x18 0x19 0x1A 0x1B 0x1C 0x1D 3 True 2 0 READ setRegister False NO_COMMAND NO_SENSOR NONE spiResponse startGyr
        (initFinishedOut, regManagerCmdInit, sensorTypeInit, spiControlInit) = sensorInit sensorFinished ACC_GYRO False MAGNETOM NO_COMMAND (0x1F, 0x38) (0x20, 0x20) (0x10, 0xC8) (0x1E, 0x38) (0x20, 0x5C) (0x22, 0x80) (0x23, 0x08) True NONE WRITE False NO_COMMAND NO_SENSOR NONE spiResponse startInit 
        (startAcc, startGyr, startInit, startMag) = unbundle $ sensor (True, False, False, False) (False, True, False, False) (False, False, True, False) (False, False, False, True) (False, False, False, False) (False, False, False, False) accFinished gyrFinished initFinished magFinished
        (regManagerCmd, sensorType, spiControlOut) = sensorSubmodulChooser NO_COMMAND NO_SENSOR NONE regManagerCmdAcc regManagerCmdGyr regManagerCmdInit regManagerCmdMag sensorTypeAcc sensorTypeGyr sensorTypeInit sensorTypeMag spiControlAcc spiControlGyr spiControlInit spiControlMag startAcc startGyr startInit startMag
        
        -------------- Buffering due to the logic-loop effect ----------------
        spiControl = register NONE spiControlOut
        accFinished = register False accFinishedOut
        gyrFinished = register False gyrFinishedOut
        magFinished = register False magFinishedOut
        initFinished = register False initFinishedOut
        ----------------------------------------------------------------------

writeSPIWrapper :: (HiddenClockReset dom gated sync) => (Signal dom SpiWriteControl) -> (Signal dom SensorOutput, Signal dom Bool)
writeSPIWrapper writeControl = (bundle (sdi, cs, spc), writeResponse)
    where
        (counter, cs, _, writeAddress, writeData, writeResponse) = sPIWriteManag doWrite (==) (==0) (==0) False 40 41 42 high low 1 True
                                                                            getWriteAddress getWriteData (+1) incmod (0:: Unsigned 6) high (0:: Unsigned 8) 
                                                                            (0:: BitVector 8) (0::BitVector 8) False writeControl
        sdi = sPIWriteSdi (==) (>=) (<=) 8 4 low 19 17 39 3 24 2 div getBit (-) low counter writeAddress writeData
        spc = sPIWriteClk (==0) (>) even (<) 40 high low 16 24 high counter

readSPIWrapper :: (HiddenClockReset dom gated sync) => (Signal dom SensorInput) -> (Signal dom SpiReadControl) -> (Signal dom SensorOutput, Signal dom SpiReadData)
readSPIWrapper sdo readControl = (bundle (sdi, cs, spc), readResponse)   
    where
        (counter, cs, _, readAddress, readResponse, _) = sPIReadManag doRead (==) (==0) (==0) (>) isEven (<) False 18 high low 1 35 34 36 True 0 customSetBit getReadAddress (+1) incmod wrapData (0::Unsigned 6) high (0::Unsigned 8) (0::BitVector 8) (READ_NOT_FINISHED) (0::BitVector 8) readControl sdo
        spc = sPIReadClk (==0) (>) even high low 34 high counter
        sdi = sPIReadSdi (==) (>=) (<=) 8 4 high 17 3 2 div getBit (-) low counter readAddress

spiWrapper :: (HiddenClockReset dom gated sync) => (Signal dom SensorInput) -> (Signal dom SpiControl) -> (Signal dom SensorOutput, Signal dom SpiOut)
spiWrapper sdo spiControl = (spiPins, spiResponseReg)  
    where
        (_, readControl, spiPins, spiResponse, writeControl) = sPI checkRead checkWrite readFinished writeFinished (low,high,high) RNone NO WNone makeOutputRead makeOutputWrite readCmd writeCmd NONE RNone (low, high, high) NO WNone readResponseReg readSpiPins spiControl writeResponseReg writeSpiPins 
        (readSpiPins,  readResponse)    = readSPIWrapper   sdo   readControl
        (writeSpiPins, writeResponse)   = writeSPIWrapper        writeControl
        ----------------------- Bundler --------------------------------
        -------------- Buffering due to the logic-loop effect ----------
        spiResponseReg    = register NO spiResponse    
        readResponseReg      = register READ_NOT_FINISHED readResponse
        writeResponseReg     = register False writeResponse
        ----------------------------------------------------------------

sensorRegisterWrapper :: (HiddenClockReset dom gated sync, KnownNat n) => 
    (Signal dom (Signed n)) -> (Signal dom Register) -> Register -> (Signal dom (Signed n))
sensorRegisterWrapper regData regType myType = regVal
    where
        regVal = sensorRegister regCmp myType 0 regData regType

sensorSelectorWrapper (inPins, sensorType) = out
    where
        (sdiIn, csIn, spcIn) = unbundle inPins
        out = bundle (sdiOut, csAGOut, csMagOut, spcOut)
        (_, csAGOut, _, csAltOut, _, csMagOut, _, sdiOut, _, spcOut) = sensorSelector isAccGyro isAlt isMag isNone high high high high high high high low low high high csIn sdiIn sensorType spcIn

registerManagerWrapper :: (HiddenClockReset dom gated sync) => (Signal dom ManagerCtr) -> (Vec 9 (Signal dom (Signed 16)))
registerManagerWrapper crt = (accx :> accy :> accz :> gyrx :> gyry :> gyrz :> magx :> magy :> magz :> Nil)
    where
        (_, regData, regType) = regManager firstHalf secondHalf REG_NONE 0 extractDat extractReg mergeReg 0 0 REG_NONE crt 
        -------------------------- Bundler --------------------------------
        internRegister  =   sensorRegisterWrapper regData regType 
        accx = internRegister REG_A_X 
        accy = internRegister REG_A_Y 
        accz = internRegister REG_A_Z 
        gyrx = internRegister REG_G_X 
        gyry = internRegister REG_G_Y 
        gyrz = internRegister REG_G_Z 
        magx = internRegister REG_M_X 
        magy = internRegister REG_M_Y 
        magz = internRegister REG_M_Z 
        --------------------------------------------------------------------
