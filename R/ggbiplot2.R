#'  PCA Plotting Function
#' 
#' @param DATES I don't remember what this is
#' @param pcobj PCA objects?
#' @param choices Defaults to \code{1:2}
#' @param scale Defaults to \code{1}
#' @param pc.biplot Defaults to \code{TRUE}
#' @param obs.scale Defaults to \code{1 - scale}
#' @param var.scale Defaults to \code{scale}
#' @param groups Defaults to \code{NULL}
#' @param ellipse Defaults to \code{FALSE}
#' @param ellipse.prob Defaults to \code{0.68}
#' @param labels Defaults to \code{NULL}
#' @param labels.size Defaults to \code{3}
#' @param alpha Defaults to \code{1}
#' @param var.axes Defaults to \code{TRUE}
#' @param circle Defaults to \code{FALSE}
#' @param circle.prob Defaults to \code{0.69}
#' @param varname.size Defaults to \code{3}
#' @param varname.adjust Defaults to \code{1.5}
#' @param varname.abbrev Defaults to \code{FALSE}
#' @param color Defaults to \code{muted("red")} # <- add new arguments to the function
#' @param linetype Defaults to \code{"solid"}
#' @param alpha_arrow Defaults to \code{1}
#' @param unit.circle.color Defaults to \code{muted("white")}
#' @param varname.color Defaults to \code{"darkred"}
#' @param point.size Defaults to \code{1}
#' @param line.size Defaults to \code{line.size}
#' @param scale_shape_manual Defaults to \code{scale_shape_manual}
#' @param scale_linetype_manual Defaults to \code{scale_linetype_manual}
#' @param axis.text.size Defaults to \code{15}
#' @param axis.title.size Defaults to \code{20}
#' @param xlim Defaults to \code{xlim}
#' @param ylim Defaults to \code{ylim}
#' @param border.size Defaults to \code{2}
#' @param point.color Defaults to \code{"black"}
#' @param pct_x_buffer Defaults to \code{0.5}
#' @param pct_y_buffer Defaults to \code{0.5}
#' Function to control graphics when plotting PCA results produced in R.
# @references edited from http://stackoverflow.com/questions/18039313/pca-scaling-with-ggbiplot, accessed 18 Sept. 15#' 
#' 
#' @export
#' @importFrom ggplot2 geom_path geom_segment geom_text

ggbiplot2 <- function (
	pcobj, 
	choices = 1:2, 
	scale = 1, 
	pc.biplot = TRUE, 
    obs.scale = 1 - scale, 
	var.scale = scale, 
	groups = NULL, 
    ellipse = FALSE, 
	ellipse.prob = 0.68, 
	labels = NULL, 
	labels.size = 3, 
    alpha = 1, 
	var.axes = TRUE, 
	circle = FALSE, 
	circle.prob = 0.69, 
    varname.size = 3, 
	varname.adjust = 1.5, 
	varname.abbrev = FALSE, 
	color = muted("red"), # <- add new arguments to the function
	linetype = "solid",
	alpha_arrow = 1,
	unit.circle.color=muted("white"),
	varname.color = "darkred",
	point.size=1,
	line.size=line.size,
	scale_shape_manual = scale_shape_manual,
	#scale_colour_manual = scale_colour_manual,
	scale_linetype_manual = scale_linetype_manual,
	axis.text.size = 15,
	axis.title.size = 20,
	xlim = xlim,
	ylim = ylim,
	border.size=2,
	point.color="black",
	pct_x_buffer=0.5,
	pct_y_buffer=0.5
) 
{
    stopifnot(length(choices) == 2)
    if (inherits(pcobj, "prcomp")) {
        nobs.factor <- sqrt(nrow(pcobj$x) - 1)
        d <- pcobj$sdev
        u <- sweep(pcobj$x, 2, 1/(d * nobs.factor), FUN = "*")
        v <- pcobj$rotation
    }
    else if (inherits(pcobj, "princomp")) {
        nobs.factor <- sqrt(pcobj$n.obs)
        d <- pcobj$sdev
        u <- sweep(pcobj$scores, 2, 1/(d * nobs.factor), FUN = "*")
        v <- pcobj$loadings
    }
    else if (inherits(pcobj, "PCA")) {
        nobs.factor <- sqrt(nrow(pcobj$call$X))
        d <- unlist(sqrt(pcobj$eig)[1])
        u <- sweep(pcobj$ind$coord, 2, 1/(d * nobs.factor), FUN = "*")
        v <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 
            1]), FUN = "/")
    }
    else if (inherits(pcobj, "lda")) {
        nobs.factor <- sqrt(pcobj$N)
        d <- pcobj$svd
        u <- predict(pcobj)$x/nobs.factor
        v <- pcobj$scaling
        d.total <- sum(d^2)
    }
    else {
        stop("Expected a object of class prcomp, princomp, PCA, or lda")
    }
    choices <- pmin(choices, ncol(u))
    df.u <- as.data.frame(sweep(u[, choices], 2, d[choices]^obs.scale, 
        FUN = "*"))
    v <- sweep(v, 2, d^var.scale, FUN = "*")
    df.v <- as.data.frame(v[, choices])
    names(df.u) <- c("xvar", "yvar")
    names(df.v) <- names(df.u)
    if (pc.biplot) {
        df.u <- df.u * nobs.factor
    }
    r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)
    v.scale <- rowSums(v^2)
    df.v <- r * df.v/sqrt(max(v.scale))
    if (obs.scale == 0) {
        u.axis.labs <- paste("standardized PC", choices, sep = "")
    }
    else {
        u.axis.labs <- paste("PC", choices, sep = "")
    }
    u.axis.labs <- paste(
		u.axis.labs, 
		"(",
		sprintf(
			"explained var.=%0.0f%%, ", 
			100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)
		),
		"EV=", 
		round(pcobj$sdev[choices]^2, 1),
		")",
		sep=""
	)
    if (!is.null(labels)) {
        df.u$labels <- labels
    }
    if (!is.null(groups)) {
        df.u$groups <- groups
    }
    if (varname.abbrev) {
        df.v$varname <- abbreviate(rownames(v))
    }
    else {
        df.v$varname <- rownames(v)
    }
    df.v$angle <- with(df.v, (180/pi) * atan(yvar/xvar))
    df.v$hjust <- with(df.v, (1 - varname.adjust * sign(xvar))/2)
    g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + xlab(u.axis.labs[1]) + 
        ylab(u.axis.labs[2]) + coord_equal()
    if (!is.null(df.u$labels)) {
        if (!is.null(df.u$groups)) {
            g <- g + geom_text(aes(label = labels, color = groups), 
                size = labels.size)
        }
        else {
            g <- g + geom_text(
						aes(label = labels), 
						size = labels.size
			)
        }
    }
    else {
        if (!is.null(df.u$groups)) {
            g <- g + geom_point(
						aes(shape=groups), 
						alpha = alpha,
						size=point.size,
						color=point.color
			)
        }
        else {
            g <- g + geom_point(alpha = alpha)
        }
    }
    if (!is.null(df.u$groups) && ellipse) {
        theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
        circle <- cbind(cos(theta), sin(theta))
        ell <- ddply(df.u, "groups", function(x) {
            if (nrow(x) <= 2) {
                return(NULL)
            }
            sigma <- var(cbind(x$xvar, x$yvar))
            mu <- c(mean(x$xvar), mean(x$yvar))
            ed <- sqrt(qchisq(ellipse.prob, df = 2))
            data.frame(sweep(circle %*% chol(sigma) * ed, 2, 
                mu, FUN = "+"), groups = x$groups[1])
        })
        names(ell)[1:2] <- c("xvar", "yvar")
        g <- g + geom_path(
					data = ell, 
					aes(color = groups, group = groups, linetype=groups), size=line.size
		)
    }
    if (var.axes) {
        if (circle) {
            theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, 
                length = 50))
            circle <- data.frame(xvar = r * cos(theta), yvar = r * 
                sin(theta))
            g <- g + geom_path(data = circle, color = unit.circle.color, 
                size = 1/2, alpha = 1/3)
        }
		g <- g + geom_segment(
					data = df.v, 
					aes(x = 0, y = 0, xend = xvar, yend = yvar), 
					arrow = arrow(length = unit(1/2, "picas")), 
					color = color, 
					linetype = linetype, 
					alpha = alpha_arrow
		)
        g <- g + geom_text(
					data = df.v, 
					aes(label = varname, x = xvar, y = yvar, angle = angle, hjust = hjust),
					color=varname.color, 
					size = varname.size#,
					#fontface = "bold"
		)
		
    }
	# PC percent variation explained
    u.axis.labs <- paste(sprintf("%0.0f%%", 
        100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))
	u.axis.labs.tot <- paste(
		sprintf("%0.0f%%", 
			round(
				100 * pcobj$sdev[choices[1]]^2/sum(pcobj$sdev^2) + 
				100 * pcobj$sdev[choices[2]]^2/sum(pcobj$sdev^2), 0
			)
		)
	)
	u.axis.labs <- paste(
		"PC Var.: ", 
		round(100 * pcobj$sdev[choices[1]]^2/sum(pcobj$sdev^2),0), 
		"+", 
		round(100 * pcobj$sdev[choices[2]]^2/sum(pcobj$sdev^2),0), 
		"=", 
		u.axis.labs.tot, sep="")
	# eigenvectors
	eigenvectors <- paste(
		"EV: ", 
		round(pcobj$sdev[1]^2, 1), 
		", ", 
		round(pcobj$sdev[2]^2, 1), 
		sep=""
	)

	g <- g + theme(legend.direction = 'horizontal', legend.position = 'top') + 
		theme_bw() +
		  theme(
			axis.line = element_line(colour = "black"),
		    panel.grid.major = element_blank(),
		    panel.grid.minor = element_blank(),
	        panel.border = element_rect(colour = "black", fill=NA, size=border.size),
		    panel.background = element_blank(),
			axis.text.x = element_text(size=axis.text.size),
			axis.text.y = element_text(size=axis.text.size)
		) +
		scale_shape_manual(values=c(scale_shape_manual)) +
		#scale_colour_manual(values = scale_colour_manual) +
		scale_linetype_manual(values = scale_linetype_manual) +
		theme(legend.position="none") +
		theme(axis.title.y = element_text(size = axis.title.size)) +
		theme(axis.title.x = element_text(size = axis.title.size)) +
		xlim(xlim) +
		ylim(ylim) +
		annotate("text", 
		x = 0.1,
			#x = xlim[2]-pct_x_buffer, 
			y = ylim[1]+pct_y_buffer, 
			label = u.axis.labs,
			size=7) +
		annotate("text", 
			x = 0.1, 
			y = ylim[1]+pct_y_buffer+0.75, 
			label = eigenvectors,
			size=7) # + # PC1
		#annotate("text", 
		#	x = xlim[1]+pct_x_buffer, 
	#		y = ylim[2]-pct_y_buffer, 
	#		label = u.axis.labs[2]) # PC2
		
    return(g)
}