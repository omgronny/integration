import System.Environment
import Functions
import List
import Integrator
import Data.Time
-- stack runghc .\main.hs

main :: IO ()
main = do


          putStrLn ("\n")          

          putStrLn ("f1 : 1 / (sqrt x)")
          putStrLn ("f2 : sinx + cosx")
          putStrLn ("f3 : 1 / (x^2 - 1)")
          putStrLn ("f4 : 3x^3 - 2x^2 - 7x - 8\n\n")
          
          putStrLn ("Which one do you want to compute?\n")
          
          n_f <- getLine

          putStrLn ("Enter the borders of the interval")
          a_char <- getLine
          let ab = wordsWhen (==' ') a_char
          
          start <- getCurrentTime
          
          -- putStrLn ("Enter the fault")
          -- f_str <- getLine
          
          putStrLn ("\n")
          
          let f = func_num (n_f)
          let a = read (get_by_index ab 0) :: Double
          let b = read (get_by_index ab 1) :: Double
          let fault = 1--read f_str :: Double
          
          putStrLn ("f(a) = " ++ (show (f a)))
          putStrLn ("f(b) = " ++ (show (f b)))

          putStrLn ("")
          
          -- let trap_result = trap f a b 10000
          -- let simpson_result = simpson f a b 6
          
          let res_trap = integration trap f a b 0.01 10000
          let tr = (get_by_index_double res_trap 0)
          let n1 = round (get_by_index_double res_trap 1) :: Integer
          
          putStrLn ("trapezium: " ++ (show tr) ++ " n=" ++ (show n1))
              
          stop <- getCurrentTime
          
          print $ diffUTCTime stop start
          
          --let res_smp = integration simpson f a b (fault * 0.01) 4
          --let sm = (get_by_index_double res_smp 0)
          --let n2 = round (get_by_index_double res_smp 1) :: Integer
          --putStrLn ("simpson: " ++ (show sm) ++ " n=" ++ (show n2))
          --putStrLn ("")
          
          
          
          
          
          
          
          