repeatedly :: Int -> a -> [a]
repeatedly n item =
  let rfn m acc =
        if m < 1
          then acc
          else rfn (m - 1) (item : acc)
   in rfn n []

repeatedly2 :: Int -> a -> [a]
repeatedly2 n item =
  if n < 1
    then []
    else item : repeatedly2 (n - 1) item

even1 :: Int -> Bool
even1 n = n == 0 || odd1 (n - 1)

odd1 :: Int -> Bool
odd1 n = n /= 0 && even1 (n - 1)
