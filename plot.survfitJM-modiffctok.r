plot.survfitMC<-function (x, estimator = c("both", "mean", "median"), which = NULL, 
    fun = NULL, conf.int = TRUE, fill.area = TRUE, col.area = rgb(255,182,193,alpha=170,max=255), #correspond à lightpink en transparent 
    col.abline = "black", col.points = "black", col.testimate = "red", col.yevo="blue", #vert
	col.rectobs="gray97", col.rectpred="antiquewhite", hachurefondpred=NULL ,
	#hachurefondpred : 1 pour mettre des hachures quand on est en noir et blanc)
	add.last.time.axis.tick = FALSE, legendgraph= TRUE,
    include.y = TRUE, main = NULL, xlab = NULL, ylab = NULL, 
    ylab2 = NULL, lty = NULL, col = NULL, lwd = NULL, pch = NULL, 
    ask = NULL, legend = FALSE, ..., cex.axis.z = 1, cex.lab.z = 1, langue=c("FR", "US"), fleches=TRUE, lty.km=2, col.km="black", horizon) 
{
 if(is.null(xlab)){xlab<-ifelse(langue=="US", "Follow-up time from transplantation (years)", "Temps de suivi depuis la transplantation (années)")}
 if(is.null(ylab)){ylab<-ifelse(langue=="US", "Patient-graft survival probability", "Probabilité de survie patient-greffon")}
 if(is.null(ylab2)){ylab2<-ifelse(langue=="US",expression(paste("Serum creatinine values "," ( ", mu, "mol/L)",sep="")), expression(paste("Valeurs de creatininemies "," (", mu, "mol/L)",sep="")))}
# expression(paste("Valeurs de créatininémies ( ", mu, "mol/L)",sep=""))
    estimator <- match.arg(estimator)
    fun <- if (!is.null(fun)) 
        match.fun(fun)
    if (is.null(which)) 
        which <- seq_along(x$summaries)
    if (conf.int && is.null(x$success.rate)) {
        warning("a confidence interval can be included only when argument", 
            "'simulate' of survfitJM() was set to TRUE.")
        conf.int <- FALSE
    }
    if (is.null(ask)) 
        ask <- prod(par("mfcol")) < length(which)
    if (ask) {
        op <- par(ask = TRUE)
        on.exit(par(op))
    }
    if (is.null(main)) {
        main <- paste("Subject", names(x$summaries))
        names(main) <- names(x$summaries)
    }
    if (is.null(xlab)) 
        xlab <- rep("Time", length(which))
    if (is.null(ylab)) {
        ylab <- if (is.null(fun)) 
            if (!include.y) 
                rep(expression(paste("Pr(", T[i] >= u, " | ", 
                  T[i] > t, ", ", tilde(y)[i](t), ")", sep = " ")), 
                  length(which))
            else rep(ifelse(langue=="US", "Survival Probability", "Probabilité de survie"), length(which))
        else rep("", length(which))
    }
    # if (is.null(ylab2)) 
        # ylab2 <- ifelse(langue=="US", "Longitudinal Outcome", "Processus longitudinal")
    if (!is.null(x$success.rate)) {
        if (is.null(col)) 
            col <- switch(estimator, both = c(2, 3, 1, 1), mean = c(2, 
                1, 1), median = c(3, 1, 1))
        if (is.null(lty)) 
            lty <- switch(estimator, both = c(1, 1, 2, 2), mean = c(1, 
                2, 2), median = c(1, 0,0))
        if (is.null(lwd)) 
            lwd <- switch(estimator, both = c(1, 1, 1, 1), mean = c(1, 
                1, 1), median = c(1, 1, 1))
    }
    else {
        col <- lty <- lwd <- 1
    }
    if (is.null(pch)) 
        pch <- 1
    for (i in seq_along(which)) {
        ii <- which[i]
        r <- x$summaries[[ii]]
        r <- if (!is.null(x$success.rate)) {
            rbind(cbind(c(0, x$last.time[ii]), matrix(1, 2, 4)), 
                r)
        }
        else {
            rbind(cbind(c(0, x$last.time[ii]), matrix(1, 2, 1)), 
                r)
        }
        if (!is.null(fun) && is.function(fun)) 
            r[, 2:ncol(r)] <- fun(r[, 2:ncol(r)])
        if (!is.null(x$success.rate) && estimator == "mean") 
            r <- r[, -3]
        if (!is.null(x$success.rate) && estimator == "median") 
            r <- r[, -2]
        if (!conf.int && !is.null(x$success.rate)) {
            exc <- c(ncol(r) - 1, ncol(r))
            r <- r[, -exc, drop = FALSE]
            col <- col[-exc]
            lty <- lty[-exc]
            lwd <- lwd[-exc]
        }
        ylim <- if (is.null(fun)) 
            c(0, 1)
        else {
            rr <- r[, -1, drop = FALSE]
            range(rr[is.finite(rr)])
        }
        if (!include.y) {
			r. <- r[r[, 1] >= lt, ] 
            matplot(r[r.[,1]<=lt+horizon, 1], r[r.[,1]<=lt+horizon, -1, drop = FALSE], type = "l", 
                col = col, lwd = lwd, lty = lty, ylim = ylim, 
                main = main[ii], xlab = xlab[i], ylab = ylab[i], 
                ...)
            if (fill.area) {
                polygon(c(r[r.[,1]<=lt+horizon, 1], rev(r[r.[,1]<=lt+horizon, 1])), c(r[r.[,1]<=lt+horizon, ncol(r) - 
                  1], rev(r[r.[,1]<=lt+horizon, ncol(r)])), col = col.area, border = "transparent")
                matlines(r[r.[,1]<=lt+horizon, 1], r[r.[,1]<=lt+horizon, -1, drop = FALSE], type = "l", 
                  col = col, lwd = lwd, lty = lty)
            }
        }
        else {
            # oldmar <- par("mar")
            # par(mar=c(5, 4, 10, 4) + 0.1,xpd=T)
            lt <- x$last.time[ii]
            r. <- r[r[, 1] >= lt, ] 
            rng <- range(min(x$obs.times[[ii]]), lt+horizon)

# plot vide			
# print(paste0("valeurs long", x$y))	
			if(lapply(x$y, max)<400){
				ordomaxy<-400
			}else{
				ordomaxy<-lapply(x$y, max)+10
			}
			plot(NULL, xlim = rng, ylim = c(0,ordomaxy),#exp(x$ry)  =0,1000, 
                xlab = xlab[i], ylab = ylab2, pch = pch, col = col.points,
                ...)		
			#	
#plot des 2 zones de couleurs		
			rect(par("usr")[1], par("usr")[3], lt, par("usr")[4], col = col.rectobs,border = NA)
			rect(lt, par("usr")[3], par("usr")[2], par("usr")[4], col = col.rectpred,border = NA, density=hachurefondpred)
		#	
#
#plot des données observées
            points(x$obs.times[[ii]],
				exp(x$y[[ii]]), xlim = rng, ylim = c(0,ordomaxy),#exp(x$ry), 
                xlab = xlab[i], ylab = ylab2, pch = 16, col = col.yevo,
                ...)
 #
#plot de la courbe predite de SCr !! A reprendre
			lines(x$fitted.times[[ii]], 
				exp(x$fitted.y[[ii]]), col = col.yevo, 
                lwd = lwd)
	#			
# ligne pointillé verticale pour dernier temps observées
			segments(x0=lt, y0=par("usr")[3], x1=lt, y1=par("usr")[4],lty = 3, col=col.abline)	
            # abline(v = lt, lty = 3, col = col.abline) #trait pointillé vertical pour couper partie long et partie surv 
#
#			
		  par(new = TRUE)#, mar=c(5, 4, 10, 4) + 0.1,usr=c(-1,1,0, 13),xpd=T)
            matplot(r.[r.[,1]<=lt+horizon, 1], #courbe de survie individuelle entre lt (last time) et lt+horizon
				r.[r.[,1]<=lt+horizon, -1, drop = FALSE], 
				type = "l", 
                col = col.testimate, lwd = lwd, lty = lty, ylim = ylim, 
                main = main[ii], xlim = rng, ylab = "", xlab = "", 
                axes = FALSE, yaxs = "i", ...)
            axis(4, las = 3, cex.axis = cex.axis.z-0.1)
#			
		tmp<-app.id[app.id$TpsEvtAns_depM12>lt,]
		tmp$TpsEvtAns_depM12[tmp$TpsEvtAns_depM12>(lt+5.01)]<-lt+5.01
		#pour pas que le dernier individu ait l'évènement sinon la courbe part en live. 
		tmp$Evt[tmp$TpsEvtAns_depM12>lt+5]<-0
		km<-survfit(Surv(TpsEvtAns_depM12,Evt==1)~1,data=tmp[which(tmp$n.creat!=0),],type="kaplan-meier")
		lines(km, conf.int=FALSE,firstx=lt, lty=lty.km, col=col.km)
		# polygon(c(km$time, rev(km$time)), c(km$lower, rev(km$upper)), col = rgb(170,182,190,alpha=70,max=255), border = "transparent")
            if (fill.area) {
                polygon(c(r.[r.[,1]<=lt+horizon, 1], rev(r.[r.[,1]<=lt+horizon, 1])), 
						c(r.[r.[,1]<=lt+horizon, ncol(r.) - 1], 
							rev(r.[r.[,1]<=lt+horizon, ncol(r.)])), 
				col = col.area, #correspond à lightpink
				border = "transparent")
                #matlines(r.[, 1], r.[, -1, drop = FALSE], type = "l", 
                 # col = col.testimate, lwd = lwd, lty = lty)
				#courbe de survie indiv = déjà tracée plus haut ! 
            }
            mtext(ylab[i], 4, 3, cex = cex.lab.z)#?
            # par(mar = oldmar, new = FALSE)
        }
        if (add.last.time.axis.tick) 
            axis(1, at = round(x$last.time[ii], 1))
        if (legend) {
            lab <- switch(estimator, both = c("Mean", "Median"), 
                mean = "Mean", median = "Median")
            if (conf.int) 
                lab <- c(lab, ifelse(langue=="US", "Lower limit", "Limite inférieure"), ifelse(langue=="US", "Upper limit", "Limite supérieure"))
            legend("left", lab, lwd = lwd, lty = lty, col = col, bty = "n", ...)
        }
		
#delimitation des bordures	- flèches	
			if(fleches==TRUE){
				if(lt>1){#car avant, ça ne sert à rien de l'afficher, c'est illisible / periode trop courte. 
					Arrows(x0=par("usr")[1], y0=par("usr")[4]+0.03, x1=lt, y1=par("usr")[4]+0.03, col = "snow4",code=3,lwd=2,arr.adj = 1,arr.length=0.3,arr.type="triangle")
					text((par("usr")[1]+lt)/2,par("usr")[4]+0.06,paste(ifelse(langue=="US", "Follow-up window", "Fenêtre de suivi")),col="snow4",cex=0.8,font=2)
				}
				Arrows(x0=lt, y0=par("usr")[4]+0.03, x1=par("usr")[2], y1=par("usr")[4]+0.03, col = "goldenrod3",code=3,lwd=2,arr.adj = 1,arr.length=0.3,arr.type="triangle")	
				text((par("usr")[2]--lt)/2-0.1,par("usr")[4]+0.06,paste(ifelse(langue=="US", "Horizon window", "Fenêtre d'horizon")),col="goldenrod3",cex=0.8,font=2)
			}


			segments(x0=par("usr")[1], y0=par("usr")[4], x1=par("usr")[2], y1=par("usr")[4], col = col.abline)	#axe horizontal supérieur
			segments(x0=par("usr")[1], y0=par("usr")[3], x1=par("usr")[2], y1=par("usr")[3], col = col.abline)	#axe abscisses
			segments(x0=par("usr")[1], y0=par("usr")[3], x1=par("usr")[1], y1=par("usr")[4], col = col.abline)	#axe gauche ordonnées
			# segments(x0=par("usr")[1], y0=par("usr")[3]+2, x1=par("usr")[1], y1=par("usr")[4]+2, col = col.abline)	
		Axis(side=1, labels=as.character(seq(0,lt+horizon,1)+1),at=seq(0,lt+horizon,1))

		
		text(lt,par("usr")[3]-0.04,sprintf("%.2f", lt+1),col="black",cex=0.6,font=2)#+1 car lt est en temps depuis les un ans post greffe 
		#rajout d'un segment avec la proba de survie
		segments(x0=rng[2],y0=r[length(r[r[,1]<=lt+horizon,1]),2], x1=rng[2]+0.3, y1=r[length(r[r[,1]<=lt+horizon,1]),2],lty=3,col=col.testimate) #segment de la courbe de survie à l axe ordonnées correpsondant
		#print(rng)
		text(rng[2]+0.62,r[length(r[r[,1]<=lt+horizon,1]),2],sprintf("%.2f", r[length(r[r[,1]<=lt+horizon,1]),2]),col=col.testimate,cex=0.8, font=2)#valeur proba survie
    }
	if(legendgraph==TRUE){
		if(langue=="US"){
			txtlegende<-c(	"Observed values", 
							"Predicted evolution", 
							"",
							"Estimated individual probability", 
							"Confidence interval (CI) at 95%", 
							"Mean crude probability\n estimated (n=2749)")#
		}else{
			txtlegende<-c("Valeurs observées", "Evolution prédite", "","Probabilité individuelle estimée", "Intervalle de confiance à 95%", "Probabilité brute\n moyenne estimée (n=2749)")
		}
		par(xpd=T)
		legend(-0.2,-0.2,legend=txtlegende,
		  col = c(col.yevo,col.yevo,"white",col.testimate,0, col.km ),fill=c("transparent","transparent","transparent","transparent", col.area,"transparent" ), border=NA,
		  lty=c(0,1,0,1,2,lty.km), 
		  lwd=c(1,1,0,1,1,1), pch=c(16,NA,NA,NA,NA,NA),bty="n",
		  cex=0.8,ncol=2,y.intersp=0.8) 	#
	}	
	#legende par défaut positionnée comme il faut pour 6 graphs : 
	# legend(-8,-0.1,legend=txtlegende,
		  # col = c("blue","blue","white","red",0, "black" ),fill=c("transparent","transparent","transparent","transparent", rgb(255,182,193,alpha=170,max=255),"transparent" ), border=NA,
		  # lty=c(0,1,0,1,2,1), 
		  # lwd=c(1,1,0,1,1,1), pch=c(16,NA,NA,NA,NA,NA),bty="n",
		  # cex=0.65,ncol=2)	
	
	
	
	
	
	#rgb(170,182,190,alpha=70,max=255) gris pour ic km 
    invisible()
	
	return(r[length(r[r[,1]<=lt+horizon,1]),2])
}

