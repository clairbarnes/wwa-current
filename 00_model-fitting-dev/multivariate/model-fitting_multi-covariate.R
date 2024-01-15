
suppressWarnings(suppressMessages({
    library(extRemes)
    library(abind)
    library(shape)
}))

prep_window <- function(rc = c(1,1), w = 4, h = 4, ...) { options(repr.plot.width = rc[2]*w, repr.plot.height = rc[1]*h, repr.plot.res = 200); par(mfrow = rc, pch = 20, ...) }
load_ts <- function(fnm, col.names) { read.csv(fnm, comment.char = "#", sep = " ", header = F, col.names = col.names) }

######################################################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################################################################################################################################################

ns_loglik <- function(pars, cov, x, dist, fittype) {

    effect <- rowSums(sapply(names(cov), function(cnm) pars[paste0("alpha_",cnm)] * cov[,cnm]))
    
    # compute nonstationary location & scale
    if(fittype == "fixeddisp") {
        const = exp(effect / pars["mu0"])
        loc = pars["mu0"] * const
        scale = pars["sigma0"] * const
    } else if(fittype == "shift") {
        loc = pars["mu0"] + effect
        scale = pars["sigma0"]
    } else {
        print(paste(fittype, "not implemented"))
        return()
    }

    # constrain variance to be strictly positive
    if(any(scale <= 0)) return(NA)
        
    # return negative log-likelihood
    if(dist == "norm") {
        return(-sum(dnorm(x, mean = loc, sd = scale, log = T)))
    } else if(dist == "gev") {
        shape = pars["shape"]
        return(-sum(devd(x, loc = loc, scale = scale, shape = shape, log = T)))
    } else {
        print(paste(dist, "not implemented"))
        return()
    }
}

fit_ns <- function(dist, type = "fixeddisp", data, varnm, covnm, lower = F, mintemps = F, ev = NA, method = "BFGS", ...) {
    
    # currently only works for distributions fully specified by mean & sd: only tested for normal, lognormal
    if(! dist %in% c("norm", "gev")) {
        print("Not yet implemented: use norm or gev")
        return()
    }
    
    # add something here to automatically handle no covariates
    
    # if looking at minimum temperatures (or minima of negative values generally), need to flip data for stable model fitting
    x <- data[,varnm]; if(mintemps) x <- -x
    cov <- data[, covnm, drop = F]
    k <- length(covnm)
    
    # fit model with appropriate number of parameters, pad if necessary
    init <- c("mu0" = mean(x), "sigma0" = sd(x), setNames(rep(0,k), paste0("alpha_", covnm)))

    if(dist == "gev") init <- c(init, "shape" = 0)
    fitted <- suppressWarnings(optim(par = init, ns_loglik, cov = cov, x = x, dist = dist, fittype = type, method = method))
    
    # if looking at minimum temperatures (or minima of negative values generally), so trend & location parameters have been flipped. This may cause some confusion so may have to modify later!
    if(mintemps) {
        fitted[["NOTE"]] <- "NB: model parameters are estimated for negative values"
        fitted$par["mu0"] <- -fitted$par["mu0"]
        fitted$par[grepl("alpha", names(fitted$par))] <- -fitted$par[grepl("alpha", names(fitted$par))]
        x <- -x
    }
            
    # attach assorted useful information
    fitted[["dist"]] <- dist
    fitted[["type"]] <- type
    fitted[["varnm"]] <- varnm
    fitted[["covnm"]] <- covnm
    fitted[["data"]] <- data
    fitted[["x"]] <- x
    fitted[["cov"]] <- cov
    
    fitted[["lower"]] <- lower               # saves having to specify every time later on
    fitted[["mintemps"]] <- mintemps         # look at maxima of 0-temps, rather than minima of observed temps
    
    if(is.na(ev)) { ev <- x[length(x)] } # event value: assume that event of interest is most recent, unless told otherwise (used in later plotting functions)
    fitted[["ev"]] <- ev

    return(fitted)
}

refit <- function(mdl, new_df) {
    fit_ns(dist = mdl$dist, type = mdl$type, data = new_df, varnm = mdl$varnm, covnm = mdl$covnm, lower = mdl$lower, mintemps = mdl$mintemps, ev = mdl$ev)
}
    
aic <- function(mdl) 2 * length(mdl$par) + 2 * mdl$value
    
######################################################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################################################################################################################################################    
    
ns_pars <- function(mdl, fixed_cov = NA) {
    
    # if no covariate value given, evaluate at all covariate values
    if(is.na(unlist(fixed_cov)[1])) fixed_cov <- mdl$cov
    
    # calculate the nonstationary parameter values
    pars <- mdl$par
    effect <- rowSums(matrix(sapply(names(fixed_cov), function(cnm) pars[paste0("alpha_",cnm)] * fixed_cov[,cnm]), nrow = nrow(fixed_cov)))
    
    if(mdl$type == "fixeddisp") {
        
        ns_const = exp(effect / pars["mu0"])
        loc = pars["mu0"] * ns_const
        scale = pars["sigma0"] * ns_const
        
    } else if(mdl$type == "shift") {
        loc = pars["mu0"] + effect
        scale = rep(pars["sigma0"], length(loc))
    } else {
        print(paste(mdl$type,"not implemented"))
        return()
    }
        
    # return the list of named parameters: location, scale, shape (if applicable)
    if("shape" %in% names(pars)) {
        return(lapply(list("loc" = loc, "scale" = scale, "shape" = rep(pars["shape"], length(scale))), unname))
    } else {
        return(lapply(list("loc" = loc, "scale" = scale), unname))
    }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

######################################################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################################################################################################################################################
### **Plotting methods**

plot_returnlevels <- function(mdl, cov, cov_cf, ev, ylim = NA, pch = 20, ylab = NA, legend_pos = "topright", main = "", 
                              xlim = c(1,10000), legend_labels = c("Present climate", "Counterfactual climate"), seed = 42, nsamp = 500, ...) {
    
    x <- mdl$x
    if(missing(ev)) { ev <- mdl$ev }
    
    rp_x <- unique(c(seq(1.1,2,0.1), seq(2,100,1), seq(100,1000,10), seq(100,1000,100), seq(1000,10000,1000)))     # return periods at which to calculate values for curves
    rp_th <- 1/seq(1,0,length.out = length(x)+2)[2:(length(x)+1)]                                                  # quantiles to map against observations to check fit

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # calculate return periods & return levels
    
    rl_curve_pres <- map_from_u(mdl, 1/rp_x, fixed_cov = cov)
    rl_curve_cf <- map_from_u(mdl, 1/rp_x, fixed_cov = cov_cf)

    rl_obs_pres <- map_from_u(mdl, map_to_u(mdl), fixed_cov = cov)
    rl_obs_cf <- map_from_u(mdl, map_to_u(mdl), fixed_cov = cov_cf)

    rp_event_pres <- 1/map_to_u(mdl, ev, fixed_cov = cov)
    rp_event_cf <- 1/map_to_u(mdl, ev, fixed_cov = cov_cf)

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # prep axes
    
    if(is.na(ylim[1])) { ylim <- range(pretty(c(x, rl_curve_pres, rl_curve_cf))) }
    if(is.na(ylab)) {ylab <- mdl$varnm}
    # if((substr(mdl$varnm,1,3) == "log") & (ylim[1] <= 0)) { ylim[1] <- 0.01 }
    
    # plot
    plot(0,type = "n", xlim = xlim, ylim = ylim, log = "x", xlab = "", ylab = "", main = main)
    mtext("Return period (years)", side = 1, line = 2.5, cex = par("cex"))
    mtext(ylab, side = 2, line = 2.5, cex = par("cex"))
    
    legend(legend_pos, legend = c(legend_labels, "Observed event"), col = c("firebrick", "blue", "magenta"), lty = 1, pch = c(pch,pch,NA), bty = "n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # return period curves
    lines(rp_x, rl_curve_pres, lwd = 2, col = "firebrick", lty = 1)       # present climate
    lines(rp_x, rl_curve_cf, lwd = 2, col = "blue", lty = 1)              # counterfactual
    
    # expected return periods vs return levels transformed to stationarity at that covariate value
    points(rp_th, sort(rl_obs_pres, decreasing = mdl$lower), col = "firebrick", pch = pch)      # present
    points(rp_th, sort(rl_obs_cf, decreasing = mdl$lower), col = "blue", pch = pch)             # counterfactual
    
    # horizontal line showing observed event, plus ticks showing return periods
    abline(h = ev, col = "magenta", lty = 2)
    suppressWarnings(rug(rp_event_pres, lwd = 3, col = "firebrick"))   # present
    suppressWarnings(rug(rp_event_cf, lwd = 3, col = "blue"))          # counterfactual
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Add confidence intervals to return periods
    
    if(!is.na(nsamp)) {
        x_ci <- c(5,10,20,50,100,200,500,1000,2000,5000,10000)
        set.seed(seed)

        mdl_df <- mdl$data[,c(mdl$varnm, mdl$covnm)]
        boot_res <- sapply(1:nsamp, function(i) {
            boot_df <- mdl_df[sample(1:nrow(mdl_df), nrow(mdl_df), replace = T),]
            tryCatch({
                boot_mdl <- refit(mdl, boot_df)
                # print(boot_mdl$par)
                c(map_from_u(boot_mdl, 1/x_ci, fixed_cov = cov), map_from_u(boot_mdl, 1/x_ci, fixed_cov = cov_cf))
            }, error = function(cond) {return(rep(NA, length(x_ci)*2))})
        })
        est_ci <- apply(boot_res, 1, quantile, c(0.025, 0.975), na.rm = T)
        
        # lines bounding confidence intervals
        # matplot(x_ci, t(est_ci[,1:length(x_ci)]), type = "l", lty = 1, lwd = 2, col = adjustcolor("firebrick", alpha = 0.3), add = T)
        # matplot(x_ci, t(est_ci[,-(1:length(x_ci))]), type = "l", lty = 1, lwd = 2, col = adjustcolor("blue", alpha = 0.3), add = T)
        
        # shaded region for confidence intervals
        polygon(x = c(x_ci, rev(x_ci)), y = c(est_ci[1,1:length(x_ci)], rev(est_ci[2,1:length(x_ci)])), density = NULL, border = NA, col = adjustcolor("firebrick", alpha = 0.1))
        polygon(x = c(x_ci, rev(x_ci)), y = c(est_ci[1,-(1:length(x_ci))], rev(est_ci[2,-(1:length(x_ci))])), density = NULL, border = NA, col = adjustcolor("blue", alpha = 0.1))
    }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_trend <- function(mdl, ev, ev_year, ylab = NA, legend_pos = "topleft", main = "", ylim = NA, lwd = 2, ...) {
    
    if(is.na(ylab)) {ylab <- mdl$varnm}
    if(is.na(ylim[1])) { ylim <- range(pretty(mdl$x)) }
    if(missing(ev)) { ev <- mdl$ev }
    if(missing(ev_year)) { ev_year <- mdl$data$year[which.min(abs(mdl$x - ev))] }
    
    plot(mdl$data$year, mdl$x, type = "S", lwd = lwd, col = adjustcolor("black", alpha = 1), xlab = "Year", ylab = ylab, main = main, ylim = ylim, ...)
    
    lines(mdl$data$year-0.5, ns_pars(mdl)$loc, col = adjustcolor("black", alpha = 0.5), lwd = lwd)
    matplot(mdl$data$year-0.5, eff_return_level(c(6,40), mdl), type = "l", lty = 1, add = T, col = adjustcolor("blue", alpha = 0.5), lwd = c(lwd, max(1,lwd -1)))
    
    points(ev_year-0.5, ev, col = "magenta", lwd = 2, pch = 0)
    
    # add legend
    legend(legend_pos, legend = c("location", "1-in-6-year event", "1-in-40-year event"), lty = 1, col = c("black", "blue", "blue"), lwd = c(lwd,lwd,max(1,lwd -1)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_covtrend <- function(mdl, xcov, cov, cov_cf, fixed_cov = NA, ev, ylim = NA, xlab = NA, ylab = NA, 
                          legend_pos = "topleft", main = "", seed = 42, nsamp = 1000, lwd = 3) {

    if(is.na(xlab)) { xlab <- toupper(xcov)}
    if(is.na(ylab)) { ylab <- mdl$varnm}
    if(is.na(ylim[1])) { ylim <- range(pretty(mdl$x)) }
    if(missing(ev)) ev <- mdl$ev
    
    x <- mdl$cov[,xcov]
    o <- order(x)
    
    plot(x, mdl$x, pch = 20, main = main, xlab = "", ylab = "", ylim = ylim, xlim = range(c(x, cov[xcov], cov_cf[xcov])))
    mtext(xlab, side = 1, line = 2.5, cex = par("cex"))
    mtext(ylab, side = 2, line = 2.5, cex = par("cex"))
    
    points(cov[,xcov], ev, col = "magenta", lwd = 2, pch = 0)
    
    # trend lines
    lines(x[o], ns_pars(mdl, fixed_cov = fixed_cov)$loc[o], lwd = 3, col = "black", lty = 1)
    lines(x[o], eff_return_level(6, mdl, fixed_cov = fixed_cov)[o], col = "blue", lwd = 3, lty = 1)
    lines(x[o], eff_return_level(40, mdl, fixed_cov = fixed_cov)[o], col = "blue", lwd = 2, lty = 1)
    
    # # get confidence interval for mu'
    # mdl_df <- mdl$data
    # set.seed(seed)
    # mu_ci <- apply(sapply(1:nsamp, function(i) {
    #     boot_df <- mdl_df[sample(1:nrow(mdl_df), nrow(mdl_df), replace = T),]
    #     tryCatch({
    #         boot_mdl <- refit(mdl, boot_df)
    #         c("mu_ev" = ns_pars(boot_mdl, fixed_cov = cov)$loc,
    #           "mu_cf" = ns_pars(boot_mdl, fixed_cov = cov_cf)$loc)
    #     }, error = function(cond) {return(rep(NA, 2))})
    # }), 1, quantile, c(0.025, 0.975), na.rm = T)
    
    # confidence interval & markers for mu' at factual & counterfactual covariates
    # lines(rep(cov[,xcov], 3), c(ns_pars(mdl, fixed_cov = cov)$loc, mu_ci[,"mu_ev"]), col = "black", lwd = 2, type = "o", pch = "_")
    # lines(rep(cov_cf[,xcov], 3), c(ns_pars(mdl, fixed_cov = cov_cf)$loc, mu_ci[,"mu_cf"]), col = "black", lwd = 2, type = "o", pch = "_")
    
    # add legend
    legend(legend_pos, legend = c("location", "1-in-6-year event", "1-in-40-year event"), lty = 1, col = c("black", "blue", "blue"), lwd = c(2,2,1))
}

######################################################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################################################################################################################################################
## MODEL FITTING RESULTS

cmdl_ests <- function(mdl, rp_2023 = 10) {
        
    pars <- mdl$par
    event_2023 <- mdl$ev
    
    cov_2023 = data.frame("gmst" = gmst_2023, "iod" = iod_2023)
    cov_hist = data.frame("gmst" = gmst_2023 - 1.2, "iod" = iod_2023)
    cov_neut = data.frame("gmst" = gmst_2023, "iod" = 0)
    cov_neuthist = data.frame("gmst" = gmst_2023 - 1.2, "iod" = 0)

    # rp_2023 <- return_period(mdl, event_2023, fixed_cov = cov_2023)
    rp_hist <- return_period(mdl, event_2023, fixed_cov = cov_hist)
    rp_neut <- return_period(mdl, event_2023, fixed_cov = cov_neut)
    rp_neuthist <- return_period(mdl, event_2023, fixed_cov = cov_neuthist)

    # hacky, but better than nothing - will give correct abs/relative value depending on fit type
    # if(is.infinite(rp_2023)) rp_2023 <- 100

    rl_hist <- eff_return_level(rp_2023, mdl, cov_hist)
    rl_neut <- eff_return_level(rp_2023, mdl, cov_neut)
    rl_neuthist <- eff_return_level(rp_2023, mdl, cov_neuthist)

    pr_hist <- prob_ratio(mdl, event_2023, cov_2023, cov_hist)
    pr_neut <- prob_ratio(mdl, event_2023, cov_2023, cov_neut)
    pr_neuthist <- prob_ratio(mdl, event_2023, cov_2023, cov_neuthist)

    dI_hist <- int_change(mdl, rp_2023, cov_2023, cov_hist, rel = F)
    dI_neut <- int_change(mdl, rp_2023, cov_2023, cov_neut, rel = F)
    dI_neuthist <- int_change(mdl, rp_2023, cov_2023, cov_neuthist, rel = F)

    rdI_hist <- int_change(mdl, rp_2023, cov_2023, cov_hist, rel = T)
    rdI_neut <- int_change(mdl, rp_2023, cov_2023, cov_neut, rel = T)
    rdI_neuthist <- int_change(mdl, rp_2023, cov_2023, cov_neuthist, rel = T)

    lss <- ns_pars(mdl, fixed_cov = cov_2023)
    disp <- lss$scale / lss$loc
    
    rp_unc <- 1/(mean(sapply(df$iod[df$year >= 1980], function(i) {
        1/return_period(mdl, x = event_2023, fixed_cov = data.frame("gmst" = gmst_2023, "iod" = i))
    })))
    rp_unc_hist <- 1/(mean(sapply(df$iod[df$year >= 1980], function(i) {
        1/return_period(mdl, x = event_2023, fixed_cov = data.frame("gmst" = gmst_2023 - 1.2, "iod" = i))
    })))
    
    pr_unc <- rp_unc_hist / rp_unc
    
    c(pars, sapply(c("disp", "event_2023", "rl_hist", "rl_neut", "rl_neuthist", "dI_hist", "dI_neut", "dI_neuthist", "rdI_hist", "rdI_neut", 
                     "rdI_neuthist", "rp_2023", "rp_hist", "rp_neut", "rp_neuthist", "rp_unc", "pr_unc", "pr_hist", "pr_neut", "pr_neuthist"), 
                   get, envir = environment()))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cmdl_boot_ci <- function(mdl, rp = 10, nsamp = 1000, seed = 42) {
    
    set.seed(seed)
    mdl_df <- mdl$data
    res <- sapply(1:nsamp, function(i) {
        # print(i)
        lres <- NA
        while(is.na(lres[1])) {
            boot_df <- mdl_df[sample(1:nrow(mdl_df), replace = T),]
            lres <- tryCatch({
                boot_mdl <- suppressWarnings(refit(mdl, boot_df))
                cmdl_ests(boot_mdl, rp = rp)
            }, error = function(cond) {return(NA)})
        }
        lres
    })
    rbind("est" = cmdl_ests(mdl, rp = rp), apply(res, 1, quantile, c(0.025, 0.975), na.rm = T))
}

