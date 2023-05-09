module Tutorial7 where

import LSystem
import Test.QuickCheck
-- import Text.PrettyPrint.GenericPretty -- ** Uncomment for Generic Pretty Challenge

pathExample = Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30

-- 1a. split
split :: Command -> [Command]
split (p :#: q) = split(p) ++ split(q)
split p = if p == Sit then [] else [p]

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join (x:[]) = x
join (x:xs) = x :#: join xs

-- 1c. equivalent
equivalent :: Command -> Command -> Bool
equivalent cmdA cmdB = split cmdA == split cmdB

-- 1d. testing join and split
prop_split_join :: Command -> Bool
prop_split_join cmd = equivalent (join(split cmd)) cmd

prop_split :: Command -> Bool
prop_split (p :#: q) = not(Sit `elem` split(p :#: q)) && not((p :#: q) `elem` split(p :#: q))
prop_split p = True

-- 2a. copy
copy :: Int -> Command -> Command
copy iter cmd
    | iter > 0 = cmd :#: (copy (iter-1) cmd)
    | otherwise = Sit

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon d = copy 5 (Go d :#: Turn 72)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon d s
    | d <= 0 || s <= 2 = Sit
    | otherwise = copy s (Go d :#: Turn (fromIntegral(360 `div` s)))


-- 3. spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command 
spiral side n step angle
    | n > 0 = (Go side :#: Turn angle) :#: (spiral (side+step) (n-1) step angle)
    | otherwise = Sit

-- 4. optimise
-- Remember that Go does not take negative arguments.

-- reduce the given command to only Go x and Turn x where x /= 0.
-- it will return Sit if the commands contains only Go 0, Turn 0 and Sit,
-- which has been defined in the Join function.
reduceCmds :: Command -> Command
reduceCmds cmd = join([x | x <- (filter (/= Sit) (split(cmd))), x /= (Go 0) && x /= (Turn 0)])

-- This turns a command with only Go x and Turn x where x /= 0 into its
-- simplified form, adding the consecutive Go and Turn commands into one.
optimise' :: Command -> Command
-- Switch brackets in case order affects the simplification.
optimise' (Go d :#: (Go e :#: Turn a)) = optimise'((Go d :#: Go e) :#: Turn a)
optimise' (Turn d :#: (Turn e :#: Go a)) = optimise'((Turn d :#: Turn e) :#: Go a)
optimise' ((Turn d :#: Go e) :#: Go a) = optimise' (Turn d :#: (Go e :#: Go a))
optimise' ((Go d :#: Turn e) :#: Turn a) = optimise' (Go d :#: (Turn e :#: Turn a))
-- Simplifying consecutive Gos and Turns into one.
optimise' (Go d :#: Go e) = optimise'(Go (d+e))
optimise' (Turn d :#: Turn e) = optimise'(Turn (d+e))
-- Base cases
optimise' (Go a) = Go a
optimise' (Turn a) = Turn a
optimise' (Turn a :#: Go b) = (Turn a :#: Go b)
optimise' (Go a :#: Turn b) = (Go a :#: Turn b)
-- Recurively simplify cmds
-- (optimise' the simplified cmd again in cases where p and q both reach
-- Go or Turn.)
optimise' (p :#: q) = optimise'(optimise'(p) :#: optimise'(q))
-- It will pass the Sit command if reduceCmds returns Sit.
optimise' Sit = Sit

-- This takes the simplified cmd and remove all Turn 0 and Go 0 cmds again
-- in cases where separate commands add up to 0.
-- If the initial reduceCmds determined the command is Sit, it will return
-- Sit as well, representing "doing nothing".
optimise :: Command -> Command
optimise cmd = reduceCmds(optimise'(reduceCmds cmd))


-- ** Optional Material

-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead x = f x
    where
        f 0 = GrabPen red :#: Go 10
        f x = g (x-1) :#: p :#: f (x-1) :#: p :#: g (x-1)
        g 0 = GrabPen blue :#: Go 10
        g x = f (x-1) :#: n :#: g (x-1) :#: n :#: f (x-1)
        p = Turn (-60)
        n = Turn 60

-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: n :#: n :#: f x :#: n :#: n :#: f x :#: n :#: n
    where
        f 0 = Go 10
        f x = f (x-1) :#: p :#: f (x-1) :#: n :#: n :#: f (x-1) :#: p :#: f (x-1)
        p = Turn (-60)
        n = Turn 60

-- 7. hilbert
hilbert :: Int -> Command 
hilbert x = l x
  where
      f = GrabPen black :#: Go 10
      l 0 = Sit
      l x = p :#: (r (x-1)) :#: f :#: n :#: (l (x-1)) :#: f :#: (l (x-1)) :#: n :#: f :#: (r (x-1)) :#: p
      r 0 = Sit
      r x = n :#: (l (x-1)) :#: f :#: p :#: (r (x-1)) :#: f :#: (r (x-1)) :#: p :#: f :#: (l (x-1)) :#: n
      n = Turn 90
      p = Turn (-90)


-- ** Challenge

-- Bonus L-Systems

peanoGosper :: Int -> Command
peanoGosper = undefined

cross :: Int -> Command
cross = undefined

branch :: Int -> Command
branch = undefined

thirtytwo :: Int -> Command
thirtytwo = undefined
