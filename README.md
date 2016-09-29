If file 'test.txt' defines a lawn-mowner file it can be launched as 
```
sbt "run [--mode=Independent|Sequential|Concurrent] text.txt"

```

Default mode is `Sequential` that moves mowers sequentially:
 1. Put all mowers into the lawn
 2. Check that all have different positions and these positions are inside of the lawn
 3. Play commands for i-th (i in 0..tasks.length) mower taking into account positions of other mowers
 
If mode is `Independent` mowers are moved absolutely independently

If mode is `Concurrent` mowers are moved sequentially:
 1. Put all mowers into the lawn
 2. Check that all have different positions and these positions are inside of the lawn
 3. Play the first command for i-th (i in 0..tasks.length) mower taking into account positions of other mowers
 4. Repeat step 3 until all mowers have no commands to replay
