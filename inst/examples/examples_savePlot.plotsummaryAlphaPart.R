## Partition additive genetic values
res <- AlphaPart(x=AlphaPart.ped, colPath="country", colBV=c("bv1", "bv2"))

## Summarize population by generation (=trend)
ret <- summary(res, by="gen")

## Plot the partitions
p <- plot(ret, ylab=c("BV for trait 1", "BV for trait 2"), xlab="Generation")

## Save the plots
tmp <- savePlot(x = p, filename="test", type="png")

## Remove the files
unlink(tmp)

