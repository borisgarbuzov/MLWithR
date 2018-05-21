x <- rbind(matrix(rnorm(100, mean = 1, sd = 0.5), , 2),
           matrix(rnorm(100, mean = 3, sd = 0.5), , 2))
y <- matrix(c(rep(1,50),rep(-1,50)))

svp <- ksvm(x,y,type="C-svc"
            # , kernel = "vanilladot"
            # , kernel = "rbfdot"
            # , kernel = "tanhdot"
            # , kernel = "anovadot"
            )
plot(svp, data = x,
     xlim = c(0, 4),
     ylim = c(0, 4))

sv1 <- x[alphaindex(svp)[[1]],]
plot(sv1,
     pch=19,
     main="SV")

