## ---- echo=TRUE----------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)

print = function() {
    
                      cat("\n","Call:","\n",
                          paste("linreg(", "formula = ", formula[2]," ", formula[1], " ", 
                                formula[3],", ", "data = ", parsedata, ")",sep = "", 
                                collapse = "\n" ),"\n","Coefficients:","\n",
                          paste(row.names(beta), sep = "  ", collapse ="  " ),"\n",
                          format(round(beta,2), justify = "centre",width = 10))
                    }
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)
linreg_mod$print()


## ----fig.width=7, fig.height=4, echo=FALSE-------------------------------
plot=function() {
              
              library(ggplot2)
              library(ggThemeAssist)
              
  #Theme             
              liu_graphic_theme<-   theme(
                
                      panel.background = element_rect(fill="white"),
                      plot.margin = unit(c(1,1,1,1), "cm"),
                      plot.caption = element_text(size=12, hjust=0.5, margin=margin(t=15)),
                      plot.title = element_text(color="#666666", face="bold", size="15",hjust=0.5),
                      panel.grid.major.y = element_blank(),
                      panel.grid.minor.y = element_blank(),
                      panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      axis.line = element_line(color= "#666666", size=0.1),
                      axis.text.x = element_text(color="#666666", size="5"),
                      axis.text.y = element_text(color="#666666", size="5"),
                      axis.title.x = element_text(color="#666666", size="12", face="bold"),
                      axis.title.y = element_text(color="#666666", size="12", face="bold"),
                      axis.ticks.x = element_line(color = "blue", size = 0.3)
                       )
               
              b<-expression(bold(sqrt(bold("Standardized Residuals"))))
              caption<-  paste("linreg(", formula[2]," ", formula[1], " ", formula[3], ")")
   #Plot1           
              df<- data.frame(FittedValues=ybar,Residulas=ebar)
              Plot1<-ggplot(df, aes(x = FittedValues, y = Residulas))+
                     geom_point(shape = 21, colour = "#666666", fill = "white", size = 3, stroke = 1.5) + ggtitle("Residual vs Fitted") +
                     geom_abline(slope = 0, intercept = 0,linetype = "dotted") +
                     geom_smooth(method = "loess",color = "red", se = FALSE) +
                     ylab("Residuals") + xlab("Fitted values")+labs(caption=caption)+
                     liu_graphic_theme
            
              df1<- data.frame(FittedValues=ybar,Residulas1=stdres)
  #Plot2        
            
              Plot2<-ggplot(df1, aes(x = FittedValues, y = Residulas1))+
                     geom_point(shape = 21, colour = "#666666", fill = "white", size = 3, stroke = 1.5) + ggtitle("Scale-Location") +
                     geom_abline(slope = 0, intercept = 0,linetype = "dotted") +
                     geom_smooth(method = "loess",color = "red", se = FALSE) +ylab(b)+
                     labs(caption=caption)+
                     xlab("Fitted values")+ 
                     liu_graphic_theme
             
                     return(list("Residual vs Fitted"= Plot1,"Scale Location"= Plot2))
            
          }        

linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)
linreg_mod$plot()

## ---- echo=TRUE----------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)
resid = function(){
                     cat("Vector of Residuals:","\n")
                     return(as.vector(round(ebar, 2)))
           }
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)
linreg_mod$resid()


## ---- echo=TRUE----------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)
pred =function(){
                     cat("Predicted values or fitted values:","\n")
                     return(as.vector(round(ybar, 2)))
          }
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)
linreg_mod$pred()

## ---- echo=TRUE----------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)
coef = function() {
                     cat("Regressions coefficients:","\n\n")
                     return(as.vector(round(beta,2)))
          }
linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)
linreg_mod$coef()

## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)
summary = function() {
                     output <- data.frame( rname = rownames(beta),
                          Estimate =round(beta,2),Std_err = round(sqrt(diag(var_beta)),2),t_value = round(t_beta, 2),
                          pr = round(p, 4))
                     output$rname <- as.character(output$rname)
                     row.names(output) <- NULL
                     output <- rbind(c("","Estimate", "Std. Error", "t value", "Pr(>|t|)"), output)
                     
                     for(i in 2:nrow(output)){
                     
                       if(output$pr[i] == 0){
                       output$pr[i] <- "***"
                     } else if(output$pr[i] > 0 & output$pr[i] <= 0.001){
                       output$pr[i] <- "**"
                     } else if(output$pr[i] > 0.001 & output$pr[i] <= 0.01){
                       output$pr[i] <- "*"
                     } else if(output$pr[i] > 0.01 & output$pr[i] <= 0.05){
                       output$pr[i] <- "."
                     } else if(output$pr[i] > 0.05 & output$pr[i] <= 0.1){
                       output$pr[i] <- " "
                     } else if(output$pr[i] > 0.1){
                       output$pr[i] <- " "
                     }
                   }
                    cat("\n Call:\n\n",
                    paste("linreg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],", ", "data = ", parsedata, ")",sep = "", collapse = "\n" ),
                    "\n\n Coefficients:\n\n")

                    for(i in 1:ncol(output)){
                    distance <- max(nchar(as.character(output[, i])), na.rm = TRUE)
                     for(j in 1:nrow(output)){
                       output[j, i] <- format(output[j, i], width=distance,justify = c("right"))
                     }
                     }
                              
                    for(i in 1:nrow(output)){
                    cat(paste(as.character(output[i, ]),collapse = " "),"\n")
                    }
                    cat("\n Residual standard error:",varian, "on", ddf, "degrees of freedom")
        
                       }

linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)
linreg_mod$summary()

## ----eval = FALSE--------------------------------------------------------
#  devtools::install_github("farhashazmeen/lab4")

