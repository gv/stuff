import System.Directory 
import System.Cmd
import Data.List (sort)

timeUnit = 4

frameTime name = (read $ '0':(filter (>= '0') (filter (<= '9') name))) ::Int

tokens (name: nextName: rest) = 
  "-delay":  show (timeUnit * (frameTime nextName - frameTime name)): name:
  tokens (nextName: rest)
  
tokens [last] = ["-delay", show (timeUnit*2), last]

tokens [] = error "No frame images"

main = do
  names <- getDirectoryContents "."
  let t = tokens $ sort $ filter (\n -> '0' == head n) names
  let cl = "-verbose": t ++ 
           words "-crop 400x300+200+0 +repage -layers optimize" ++ ["r.gif"]
  print (unwords cl)    
  rawSystem "d:/programs/ImageMagick/convert.exe" cl
  rawSystem "d:/programs/ImageMagick/identify.exe" 
    (words "-format %T\\n r.gif")

                                                                  
