module Functions where

import Clash.Prelude

------------------------ TYPES --------------------------

type SensorInput    =   Bit                         -- SDO
type SensorOutput   =   (Bit, Bit, Bit)             -- SDI, CS, SPC
type SensorRealOutput   = (Bit, Bit, Bit, Bit)      -- SDI, CS_AG, CS_M, SPC

type SignalEnemies dom  =  Vec 4 (Signal dom Enemy) 
data Gamemode   = Score_Mode | Radar_Mode | Cockpit_Mode
type Angle      = Unsigned 8
type Color      = BitVector 3
type Radius     = Unsigned  8
type Score      = Unsigned  7
type LEDPoint   = (Unsigned 5, Unsigned 5, BitVector 3)
type Enemy      = (Angle, Radius, Color)

type SensorAdress       = BitVector 8
type SensorRegister     = BitVector 8

data SpiWriteControl    = WNone | Write (SensorAdress, SensorRegister)

data SpiReadControl     = RNone | Read SensorAdress     
data SpiReadData        = READ_NOT_FINISHED | READ_FINISHED SensorRegister  

data SensorType     = ACC_GYRO | MAGNETOM | ALTITUDE | NO_SENSOR
data SensorCommand  = ACC | GYRO | MAG | NO_MODULE

data Register = REG_NONE | REG_A_X | REG_A_Y | REG_A_Z | REG_G_X | REG_G_Y | REG_G_Z | REG_M_X | REG_M_Y | REG_M_Z 
data ManagerCtr = NO_COMMAND | SET (Register, Unsigned 1, BitVector 8)
data SpiControl     =   NONE    | WRITE (SensorAdress, SensorRegister) | READ SensorAdress
data SpiOut         =   NO      | FINISHED SensorAdress | RECIEVED (SensorAdress, SensorRegister)

$(decLiteralD 5632)

--------------- SENSOR FUNCTIONS -----------------------

isAlt ALTITUDE  = True
isAlt _         = False

isMag MAGNETOM  = True
isMag _         = False

isAccGyro ACC_GYRO  = True
isAccGyro _         = False

isNone NO_SENSOR = True
isNone _         = False

writecommand a b = Write (a,b)

submodulefinished ACC  (x , _ , _) = x
submodulefinished GYRO (_ , x , _) = x
submodulefinished MAG  (_ , _ , x) = x
submodulefinished _    (_ , _ , _) = False

readFinished READ_NOT_FINISHED = False
readFinished _                 = True

writeFinished x = x

setRegister modtype (num::Unsigned 3) (RECIEVED (_,dat)) = SET (getReg(modtype, shift num (-1)), (resize(num .&. 0x1 )), dat)
setRegister _ _ _ = NO_COMMAND
getReg (ACC, 0)  = REG_A_X
getReg (ACC, 1)  = REG_A_Y
getReg (ACC, 2)  = REG_A_Z
getReg (GYRO, 0) = REG_G_X
getReg (GYRO, 1) = REG_G_Y
getReg (GYRO, 2) = REG_G_Z
getReg (MAG, 0)  = REG_M_X
getReg (MAG, 1)  = REG_M_Y
getReg (MAG, 2)  = REG_M_Z
getReg _         = REG_NONE

sensorFinished NO = False
sensorFinished _  = True
-------------------- GAME CONSTANTS ---------

sizex = 32
sizey = 32
hsizex = 16
hsizey = 16
hairlinex = 16
hairliney = 16

redcolor = 0b100
blackcolor = 0b000
whitecolor = 0b111

enemycount = 4

startradius = 0xFF

----------------- GAMERELATED FUNCTIONS --------------------

isscoremode Score_Mode  = True
isscoremode _           = False

isradarmode Radar_Mode  = True
isradarmode _           = False

iscockpitmode Cockpit_Mode  = True
iscockpitmode _           = False

isgameover :: Radius -> Bool
isgameover x = x <= 8

ismiddel :: Unsigned 5 -> Unsigned 5 -> Bool
ismiddel x y = (x == 15 || x == 16) && (y == 15 || y == 16)   

sfixedtoUnsigned :: SFixed 10 5 -> Unsigned 5
sfixedtoUnsigned n = resize (unpack (pack ((resize (shiftR (unSF n) 5) :: Signed 10))) :: Unsigned 10)

unsignedtoSFixed :: Unsigned 8 -> SFixed 10 5
unsignedtoSFixed n = sf d5 (unpack (pack (zeroExtend n :: Unsigned 10) ++# 0))

cartesianx :: SFixed 10 5 -> Radius -> Unsigned 5
cartesianx cos rad = sfixedtoUnsigned(cos * (unsignedtoSFixed rad / 16) + 16)

cartesiany :: SFixed 10 5 -> Radius -> Unsigned 5
cartesiany sin rad = sfixedtoUnsigned(sin * (unsignedtoSFixed rad / 16) + 16)

hitenemy :: Angle -> Radius -> Angle -> Bool
hitenemy angle radius rotation = (angle - rotation) < s || (angle - rotation) > 255 - s
    where 
        s = if radius == 0 then startradius else startradius `div` radius -- object width
 
hitcoord x y radius angle rotation = hit_x && hit_y
    where
        object_width    = if radius > 0 then startradius `div` radius else startradius
        relativ_angle   = angle - rotation + resize(x) - 16
        hit_x           = relativ_angle < object_width || relativ_angle > 255 - object_width     
        hit_y           = resize (if y > 16 then y - 16 else 16 - y) < object_width  

getenemyradius enemies index = r
    where
        (a, r, c) = enemies !! (index `mod` enemycount)    

getenemyangle enemies index = a
    where
        (a, r, c) = enemies !! (index `mod` enemycount)

getenemycolor enemies index = c
    where
        (a, r, c) = enemies !! (index `mod` enemycount)

newrand :: Angle -> Angle
newrand a = a + 2^7 - 7

triggerreset :: Unsigned 3 -> Angle -> (Vec 4 (Bool, Angle)) 
triggerreset 0 a = (True, a) :> (False, 0) :> (False, 0) :> (False, 0) :> Nil
triggerreset 1 a = (False, 0) :> (True, a + 50) :> (False, 0) :> (False, 0) :> Nil
triggerreset 2 a = (False, 0) :> (False, 0) :> (True, a + 126) :> (False, 0) :> Nil
triggerreset 3 a = (False, 0) :> (False, 0) :> (False, 0) :> (True, a + 189) :> Nil
triggerreset _ a = triggerresetnone

triggerresetall :: Angle -> (Vec 4 (Bool, Angle))
triggerresetall a = (True, a + 000) :> (True, a + 050) :> (True, a + 126) :> (True, a + 189) :> Nil 

triggerresetnone :: (Vec 4 (Bool, Angle))
triggerresetnone = (False, 0) :> (False, 0) :> (False, 0) :> (False, 0) :> Nil

ticknomove :: (Vec 4 Bool)
ticknomove = False :> False :> False :> False :> Nil 

tickmove :: (Unsigned 11, Unsigned 13) -> (Vec 4 Bool)
tickmove (0, c) = (c == 0) :> (c == 0 || c == 2^12) :> (c == 0 || c == 2^12) :> (c == 0 || c == 2^11 || c == 2^12 || c == 2^12 + 2^11) :> Nil
tickmove (_, c) = ticknomove

incmoveclock :: (Unsigned 11, Unsigned 13) -> Score -> (Unsigned 11, Unsigned 13)   
incmoveclock (0, c) score = (1, c + 1)
incmoveclock (p, c) score = (if p > 2^10 then 0 else p + 1 + (shiftR (resize score) 3), c)

fastestEnemy :: (Vec 4 (Angle, Radius, Color)) -> Color
fastestEnemy ((_, r1, c1) :> (_, r2, c2) :> (_, r3, c3) :> (_, r4, c4) :> Nil) = result
    where
        r1' = r1
        r2' = shiftR r2 1
        r3' = shiftR r3 1
        r4' = shiftR r4 2
        (t1,tr1) = if r1' < r2' then (c1, r1')  else (c2, r2')
        (t2,tr2) = if r3' < r4' then (c3, r3')  else (c4, r4') 
        result   = if tr1 < tr2 then t1         else t2

-------------------------- SPI FUNCTIONS ---------------------

checkRead (READ x) = True
checkRead   _      = False

checkWrite (WRITE x) = True 
checkWrite  _         = False

extractAddress (WRITE (a,b)) = a
extractAddress (READ a)      = a
extractAddress _             = 0

doWrite WNone     = False
doWrite (Write _) = True

getWriteData (Write (x,y)) = y
getWriteData  _            = 0

getWriteAddress (Write (x,y)) = x
getWiteAddress _             = 0

makeOutputWrite True (WRITE (x,y)) = FINISHED x
makeOutputWrite _ _ = NO

makeOutputRead (READ_FINISHED x) (READ y) = RECIEVED (y,x)
makeOutputRead _ _ = NO 

readCmd (READ a) = Read a
readCmd _        = RNone

writeCmd (WRITE (a,b)) = Write (a,b)
writeCmd _             = WNone


getBit v 0 = if testBit v 0 then high else low 
getBit v 1 = if testBit v 1 then high else low 
getBit v 2 = if testBit v 2 then high else low 
getBit v 3 = if testBit v 3 then high else low 
getBit v 4 = if testBit v 4 then high else low 
getBit v 5 = if testBit v 5 then high else low 
getBit v 6 = if testBit v 6 then high else low 
getBit v 7 = if testBit v 7 then high else low 
getBit v _ = low

isEven x = (x `mod` 2) == 0

customSetBit inp newBit = (shiftL inp 1)  .|. ((resize . pack) newBit)

doRead RNone    = False
doRead (Read _) = True

getReadAddress (Read x) = x
getReadAddress _        = 0

wrapData _ False = READ_NOT_FINISHED
wrapData x True  = READ_FINISHED x

unwrapData READ_NOT_FINISHED = Nothing
unwrapData (READ_FINISHED x) = Just x

filterRec (RECIEVED _) = True
filterRec _ = False
unpackRec (RECIEVED (x,y)) = Just (x,y)
unpackRec _ = Nothing

--------------- CONVERTER FUNCTIONS and CONSTANTS --------------- 

norotation gx gy gz = abs(gx) + abs(gy) + abs(gz) < 0x700
resetthreshhold = 0x5000                                            -- a threshold value for the Z-accelerometer to trigger a reset (Note: it contains the gravity)
shotthreshhold  = 0x1000                                            -- a threshold value for the Z-accelerometer to trigger a shot
gamemodeswitch  = shiftL 0x20 20
gamemodenoise x = abs(x) < 0x100 
gyronoise x     =  abs(x) <= 0x80 
addrotation old correction gyro = old + correction + resize gyro
subrotation old correction gyro = old + correction - resize gyro
roationcaluclatorprescaler = 751
gyrocorrection = 0x50

scalerotation :: (Signed 28) -> Angle   
scalerotation n = unpack (resize (shiftR (pack n) 20))

------------------ BASIC (general) FUNCTIONS ------------------

on :: Bool -> Bool
on x = x

-- Arithmetic operations
incmod x m = if x == m - 1 then 0 else x + 1 --(x + 1) `mod` m

-------------------- REGISTER FUNCTIONS --------------------------------

-- TODO: make this equality check great again
regCmp :: Register -> Register -> Bool 
regCmp REG_NONE REG_NONE = True
regCmp REG_A_X REG_A_X = True
regCmp REG_A_Y REG_A_Y = True
regCmp REG_A_Z REG_A_Z = True
regCmp REG_G_X REG_G_X = True
regCmp REG_G_Y REG_G_Y = True
regCmp REG_G_Z REG_G_Z = True
regCmp REG_M_X REG_M_X = True
regCmp REG_M_Y REG_M_Y = True
regCmp REG_M_Z REG_M_Z = True
regCmp _        _      = False

firstHalf (SET (_, 0, _)) = True
firstHalf _             = False

secondHalf (SET (_, 1, _)) = True
secondHalf _             = False

extractDat (SET (_ , _, x)) = x
extractDat _              = 0x0

extractReg (SET (x, _, _)) = x
extractReg _             = REG_NONE

mergeReg x buf = unpack $ (shiftL (resize x) 8) .|. (resize buf)

--------------------- RAM-OPERATIONS -----------------------------

writeramnone = Nothing
writeram :: Color -> Unsigned 5 -> Unsigned 5 -> (Maybe (Unsigned 10, Color))
writeram color x y = Just(resize x + resize y * sizex, color)

rampos1 :: Unsigned 5 -> Unsigned 4 -> (Unsigned 10)
rampos1 x y = resize x + resize y * sizex

rampos2 :: Unsigned 5 -> Unsigned 4 -> (Unsigned 10)
rampos2 x y = resize x  +  (hsizex + resize y) * sizex 

ramposR :: Unsigned 5 -> Unsigned 5 -> (Unsigned 10)
ramposR x y = resize x + resize y * sizex 


getRamPos :: Score -> Unsigned 5 -> Unsigned 5 -> Unsigned 13
getRamPos s x y = p
    where
        right = x < 16
        corrX = if right then x else x - 16
        p = 511 - 16 * (resize y) - (resize corrX) + scoreOffset
        scoreOffset = if right then rightOff rightDigit else leftOff leftDigit
        rightOff :: Unsigned 4 -> Unsigned 13
        rightOff digit = 512 + 512 * (resize digit)
        leftOff digit = if digit == 0 then 0 else rightOff digit
        (rightDigit :: Unsigned 4) = resize $ s `mod` 10  
        (leftDigit :: Unsigned 4) = resize $  s `div` 10
