################################################################################
################################################################################
############################ More Data Generators  #############################
################################################################################
################################################################################

# This file defines generators only; it does not write data.
# mvtnorm is called explicitly via its namespace.

####################### Correlated Feature Generator ###########################

correlatedFeatureGenerator <- function(ndim, mu, nCov = ndim){
  force(mu)
  covMat = cov(matrix(rnorm(ndim*nCov), ncol = ndim))
  res = list(mu=mu,
             covMat = covMat,
             sample = function(n){
               mvtnorm::rmvnorm(n, mu, covMat)
             }
  )
  res
} 


############ Correlated Bounded Feature Generator ##############################

correlatedBoundedFeatureGenerator <- function(ndim, mu, nCov = ndim,
                                              bound = mu,  side = 'low', handling = 'folding'){
  force(mu)
  covMat = cov(matrix(rnorm(ndim*nCov), ncol = ndim))
  res = list(mu=mu,
             covMat = covMat,
             sample = function(n){
               x = mvtnorm::rmvnorm(n, mu, covMat)
               if(handling == 'folding'){
                 if(side == 'low'){
                   x =  sapply(1:ndim, \(i) bound[i] + abs(x[,i] - bound[i]))
                 }else if(side == 'up'){
                   x = sapply(1:ndim, \(i) bound[i] - abs(x[,i] - bound[i]))
                 }
               }
               if(handling == 'wall'){
                 for(i in 1:ndim){
                   if(side == 'low'){
                     idx = (x[,i] < bound[i])
                   }else if(side == 'up'){
                     idx = (x[,i] > bound[i])
                   }                   
                   x[idx,i] = bound[i]  
                   
                 }

               }
               matrix(x, ncol = ndim)
             }
  )
  res
} 



########################### Point Mass Feature generator #######################


point_mass_feature_generator <- function(muNorm, frctMass = .3, massPoint = muNorm){
  res = list(muNorm=muNorm, frctMass = frctMass, massPoint=massPoint,
             sample = function(n){
               x = rnorm(n, muNorm)
               x[runif(n) < frctMass] = massPoint
               matrix(x, ncol=1)
             })
  res
}

# Backward-compatible alias used by the original simulation script.
pointMaßFeatureGenerator <- point_mass_feature_generator


############################### Bounded Feature Generator ######################

boundedFeatureGenerator <- function(muNorm, bound = muNorm,  side = 'low', handling = 'folding'){
  res = list(muNorm=muNorm, bound = bound, side=side, handling=handling,
             sample = function(n){
               x = rnorm(n, muNorm)
               if(handling == 'folding'){
                 if(side == 'low'){
                   x =  bound + abs(x - bound)
                 }else if(side == 'up'){
                   x = bound - abs(x - bound)
                 }
               }
               if(handling == 'wall'){
                 if(side == 'low'){
                   x[x < bound] = bound  
                 }else if(side == 'up'){
                   x[x > bound] = bound  
                 }
               }
               matrix(x, ncol=1)
             })
  res
}

####################### binomFeature Generator #################################

binomFeatureGenerator <- function(size = 20,  p = .5,  shift = size*p,
                                  scale = sqrt(size*p*(1-p))){
  res = list(size=size, p = p, shift = shift, scale = scale,
             sample = function(n){
               x = rbinom(n, prob = p, size = size)
               x = x - shift
               matrix(x/scale, ncol=1)
             })
  res
}

########################## linear Depended Feature Genrator ####################

makeLinearDependedFeatureGenerator <- function(ndim = 4, factRange = c(-5,5),
                                               ynoise = 0){
  facts = runif(ndim-1, factRange[1], factRange[2])
  facts = c(1, facts)
  res = list(ndim = ndim, factRange = factRange, ynoise = ynoise,
             facts =facts,
             sample = function(n){
               x = rnorm(n)
               sapply(1:ndim, \(i){
                 x*facts[i] +rnorm(n, 0, ynoise)
               })
             }
  )
}

############################ Noise Generator ###################################

makeNoiseGenerator <- function(ndim = 4, ynoise){
  res = list(ndim = ndim, ynoise = ynoise,
             sample = function(n){
               sapply(1:ndim, \(i){
                 rnorm(n, 0, ynoise)
               })
             }
  )
}

############################ UNIF GENERATOR ####################################


makeUnifGenerator <- function(ndim = 4, limits = c(-4,4)){
  res = list(ndim = ndim, limits = limits,
             sample = function(n){
               sapply(1:ndim, \(i){
                 runif(n, limits[1], limits[2])
               })
             }
  )
}

############################# CorBinomGenerator ################################

makeCorBinomGenerator <- function(cor = .7, size = 20, p = 0.5, normalize=F){
  res = list(cor = cor, size = size,
             sample = function(n){
               qval = qnorm(p)
               sigma = matrix(c(1, cor, cor, 1), ncol = 2)
               berCors = lapply(1:size, function(i){
                 mvtnorm::rmvnorm(n, c(0, 0), sigma = sigma) > qval
               })
               res = Reduce('+', berCors)
               if(normalize){
                 res = (res - p * size) / sqrt(size * p * (1 - p))
               }else{
                 res / 1 # force num
                 
               }
             })
  
}

################################################################################
################################################################################
######################### COMBINDE FEATURE GENERATOR ###########################
################################################################################
################################################################################

combineGenerators <- function(genList){
  res = list(gens = genList,
             sample = function(N){
               do.call(cbind, lapply(genList, \(gen) gen$sample(N)))
             })
}



########################### Classifications Generator ##########################



makeClassificationGenerator <- function(genList, classLabels = paste0('c',1:length(genList))){
  ### Safety check 
  
  classLabels = factor(classLabels)
  testSample = sapply(genList, \(gen) dim(gen$sample(10)))
  if(length(unique(testSample[2,])) != 1){
    stop('Generator Sample in different dimensions')
  }
  outDim = testSample[2,1]
  res = list(gens = genList, outDim = outDim, classLabels = classLabels,
             sample = function(n, p = rep(1/length(genList), length(genList))){
               classes = sample(1:length(genList), n, replace = T, prob = p)
               x = do.call(rbind, lapply(1:length(genList), \(i) genList[[i]]$sample(sum(classes == i))))
               data.frame(x=x, y = classLabels[sort(classes)])
             })
}


############################## make Logit Classification #######################

makeLogitClassificationGenerator <- function(xGen, features = c(1),
                                             aggFun = sum){
  testSample = xGen$sample(5000)
  relScala = apply(testSample[, features, drop=F], 1, aggFun)
  q50 = quantile(relScala, .5)
  res = list(xGen = xGen, q50 = q50, 
             sample = function(n){
               x = xGen$sample(n)
               relScala = apply(x[, features, drop=F], 1, aggFun)
               probs = 1/(1+exp(relScala - q50))
               y = rep('c1', n)
               y[runif(n) > probs] = 'c2'
               data.frame(x=x, y = factor(y, levels = c('c1', 'c2')))
             })
}



################################################################################

quickComparing <- function(makeClassification, nSample, nEvals, algList){
  
  ccrMat = matrix(NA, ncol=5, nrow = nEvals)
  
  for(i in 1:nEvals){
    
    classGen = makeClassification()
    train = classGen$sample(nSample)
    test =  classGen$sample(2000)
    
    res = sapply(algList, \(alg){
      
      mod = alg(x=train %>% select(-y),
                y = train$y)
      
      preds = predict(mod, test %>% select(-y))$predictions
      mean(preds == test$y)
    })
    ccrMat[i, ] = res
  }
  ccrMat
}

################################################################################




makeXORClassificationGenerator <- function(xGen){

  res = list(xGen = xGen, 
             sample = function(n){
               x = xGen$sample(n)
               tmp = floor(x)
               y = c('c1', 'c2')[1 + rowSums(tmp) %% 2]
               data.frame(x=x, y = factor(y, levels = c('c1', 'c2')))
             })
  res
}


################################################################################

makeCircularClassification <- function(dim, dists = runif(2, 0, 2)){
  res = list(dim = dim, dists = dists,
             sample = function(n){
               x = sapply(1:dim, \(i) runif(n, -1, 1))
               x = x / sqrt(rowSums(x^2))
               y = c(rep('c1', n/2), rep('c2', n/2))
               x[y == 'c1', ] = x[y == 'c1', ] * dists[1]
               x[y == 'c2', ] = x[y == 'c2', ] * dists[2]
               data.frame(x=x, y = factor(y, levels = c('c1', 'c2')))
             })
  res
}


################################################################################


makeOneHotEncFeats <- function(noCats = 5){
  probs = runif(noCats)
  probs = probs / sum(probs)
  res = list(probs = probs, noCats = noCats,
             sample = function(n){
               tmp = sample(1:noCats, size = n, prob = probs, replace = T)
               x = matrix(0, ncol = noCats, nrow = n)
               x[cbind(1:n, tmp)] = 1
               x
             })
}
