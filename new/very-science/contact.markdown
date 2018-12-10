---
title: Talk to me!
---

This is a test post with some inline Haskell. Consequatur est quas id vero aut perspiciatis quod dolor. Libero non ut modi aut reiciendis dolorem expedita. Tenetur sed consequuntur quis debitis eos rerum vel. Dolorem consequatur ut qui laboriosam voluptatibus assumenda reprehenderit dolore. Est quae voluptatem deleniti quod voluptas. [Sunt harum ullam dolore minus](https://wikipedia.org) est dicta deserunt. The `draws` function!

```haskell
draws :: [Input] -> Gen (Variant, [Input])
draws inputs = go [inputs]
  where
    -- Mutually recursive:
    go, inwardFrom :: [[Input]] -> Gen (Variant, [Input])

    go levels =
      oneof                               -- 50% choice between:
        [ return (mempty, concat levels)  -- stop consuming input, or
        , inwardFrom levels ]             -- keep consuming input

    inwardFrom levels =
      case levels of
        [            ] -> return mempty         -- if no more input: stop
        [  ] : outside -> inwardFrom outside    -- if nothing here: backtrack
        here : outside -> do                    -- if something here: go deeper
          (Input v inside, here') <- pick here
          vary v $ do
            (entropy, levels') <- go (inside : here' : outside)  -- back to 'go'
            return (v <> entropy, levels')

    -- Pick a random list element and the remaining list
    pick :: [a] -> Gen (a, [a])
    pick as = do
      index <- choose (0, length as - 1)
      let (before, picked : after) = splitAt index as
      return (picked, before ++ after)
```

And some more text... [Consequatur est quas id vero](http://www.google.com) aut perspiciatis quod dolor. Libero non ut modi aut reiciendis dolorem expedita. Tenetur sed consequuntur quis debitis eos rerum vel. Dolorem consequatur ut qui laboriosam voluptatibus assumenda reprehenderit dolore. Est quae voluptatem deleniti quod voluptas. Sunt harum ullam dolore minus est dicta deserunt.
