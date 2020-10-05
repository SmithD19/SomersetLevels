



library(Hmsc)

PresenceAbsence

Grad <- constructGradient(output, focalVariable = "dissolved_oxygen")

predY1 <- predict(output, Gradient = Grad, expected = TRUE)
predY2 <- predict(output, Gradient = Grad, expected = FALSE)

plot(predY1)

bind_rows(predY1)

plotGradient(output, Grad, predY = predY2, measure = "")

plotGradient(output, Grad, pred=predY2, measure="Y", showData = TRUE, jigger = 0.05, index = 4)
