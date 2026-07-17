################################################################################
################################################################################
################################################################################
###################### Create Complex Demonstration Data ########################
################################################################################
################################################################################

########################## Make Binom Datasets #################################

set.seed(1L)

generator_file <- file.path(
  "scripts", "data_creation", "02_complex_data_generators.R"
)

if (!file.exists(generator_file)) {
  generator_file <- "02_complex_data_generators.R"
}

if (!file.exists(generator_file)) {
  stop("Could not find the complex-data generator script: ", generator_file)
}

source(generator_file)

output_dir <- file.path("data", "demonstration", "complex_data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

createDataSet <- function(
    ClassProbGen,
    nSets,
    nTrain,
    nTest,
    name,
    path = output_dir
) {
  for (i in seq_len(nSets)) {
    datGen <- ClassProbGen()
    train <- datGen$sample(nTrain)
    test <- datGen$sample(nTest)

    saveRDS(
      list(train = train, test = test, datGen = datGen),
      file.path(path, paste0(name, "_", i, ".rds"))
    )
  }
}

########################## Natural Binom #######################################

makeBinomClassProb =function(){
  multiCorFeatGen1 = combineGenerators(
    lapply(1:5, \(i)binomFeatureGenerator(
      size = 20, p = runif(1, .45, .55), scale=1, shift = 0)))
  multiCorFeatGen2 = combineGenerators(
    lapply(1:5, \(i)binomFeatureGenerator(
      size = 20, p = runif(1, .45, .55), scale=1, shift = 0)))
  makeClassificationGenerator(list(multiCorFeatGen1, multiCorFeatGen2))
}

createDataSet(makeBinomClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'naturalBinom5')

makeBinomClassProb =function(){
  multiCorFeatGen1 = combineGenerators(
    lapply(1:10, \(i)binomFeatureGenerator(
      size = 20, p = runif(1, .45, .55), scale=1, shift = 0)))
  multiCorFeatGen2 = combineGenerators(
    lapply(1:10, \(i)binomFeatureGenerator(
      size = 20, p = runif(1, .45, .55), scale=1, shift = 0)))
  makeClassificationGenerator(list(multiCorFeatGen1, multiCorFeatGen2))
}

createDataSet(makeBinomClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'naturalBinom10')


########################### Scaled Binom #######################################

makeBinomClassProb =function(){
  multiCorFeatGen1 = combineGenerators(
    lapply(1:5, \(i)binomFeatureGenerator(
      size = 20, p = runif(1, .45, .55), shift = 0)))
  multiCorFeatGen2 = combineGenerators(
    lapply(1:5, \(i)binomFeatureGenerator(
      size = 20, p = runif(1, .45, .55), shift = 0)))
  makeClassificationGenerator(list(multiCorFeatGen1, multiCorFeatGen2))
}

createDataSet(makeBinomClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'scaledBinom5')

makeBinomClassProb =function(){
  multiCorFeatGen1 = combineGenerators(
    lapply(1:10, \(i)binomFeatureGenerator(
      size = 20, p = runif(1, .45, .55), shift = 0)))
  multiCorFeatGen2 = combineGenerators(
    lapply(1:10, \(i)binomFeatureGenerator(
      size = 20, p = runif(1, .45, .55), shift = 0)))
  makeClassificationGenerator(list(multiCorFeatGen1, multiCorFeatGen2))
}

createDataSet(makeBinomClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'scaledBinom10')

############################# Logit class Problems #############################

makeLogitClassProb = function(){
  multiCorFeatGen1 = combineGenerators(list(correlatedFeatureGenerator(ndim = 2,  rnorm(2), nCov= 3),
                                            correlatedFeatureGenerator(ndim = 3,  rnorm(3), nCov= 4)
                                            ))
  makeLogitClassificationGenerator(multiCorFeatGen1, features = c(1,3))
}

createDataSet(makeLogitClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'logitCor5')

makeLogitClassProb =function(){
  multiCorFeatGen1 = combineGenerators(lapply(1:5, \(i)correlatedFeatureGenerator(ndim = 2,  rnorm(2), nCov= 2)))
  makeLogitClassificationGenerator(multiCorFeatGen1, features = seq(1, 10, 2))
}
createDataSet(makeLogitClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'logitCor10')

############################### Bounded Features ###############################

makeBoundClassProb =function(){
  multiBoundGen1 = combineGenerators(
    lapply(1:5, \(i)boundedFeatureGenerator(muNorm = 0, bound = runif(1, -3, -.5),
                                            side = 'low')))
  multiBoundGen2 = combineGenerators(
    lapply(1:5, \(i)boundedFeatureGenerator(muNorm = 0, bound = runif(1, .5, 3),
                                            side = 'up')))
  makeClassificationGenerator(list(multiBoundGen1,multiBoundGen2))
}
createDataSet(makeBoundClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'boundedFeats5')

makeBoundClassProb =function(){
  multiBoundGen1 = combineGenerators(
    lapply(1:10, \(i)boundedFeatureGenerator(muNorm = 0, bound = runif(1, -3, -.5),
                                            side = 'low')))
  multiBoundGen2 = combineGenerators(
    lapply(1:10, \(i)boundedFeatureGenerator(muNorm = 0, bound = runif(1, .5, 3),
                                            side = 'up')))
  makeClassificationGenerator(list(multiBoundGen1,multiBoundGen2))
}
createDataSet(makeBoundClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'boundedFeats10')

############################ Bounded Wall Features ##############################

makeBoundClassProb =function(){
  multiBoundGen1 = combineGenerators(
    lapply(1:5, \(i)boundedFeatureGenerator(muNorm = 0, bound = runif(1, -3, -.5),
                                            side = 'low', handling = 'wall')))
  multiBoundGen2 = combineGenerators(
    lapply(1:5, \(i)boundedFeatureGenerator(muNorm = 0, bound = runif(1, .5, 3),
                                            side = 'up', handling = 'wall')))
  makeClassificationGenerator(list(multiBoundGen1,multiBoundGen2))
}
createDataSet(makeBoundClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'boundedWallFeats5')

makeBoundClassProb =function(){
  multiBoundGen1 = combineGenerators(
    lapply(1:10, \(i)boundedFeatureGenerator(muNorm = 0, bound = runif(1, -3, -.5),
                                             side = 'low', handling = 'wall')))
  multiBoundGen2 = combineGenerators(
    lapply(1:10, \(i)boundedFeatureGenerator(muNorm = 0, bound = runif(1, .5, 3),
                                             side = 'up', handling = 'wall')))
  makeClassificationGenerator(list(multiBoundGen1,multiBoundGen2))
}
createDataSet(makeBoundClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'boundedWallFeats10')

############################### Point Mass Features ############################

makePMClassProb =function(){
  multiPointMassGen1 = combineGenerators(lapply(1:5, \(i)point_mass_feature_generator(muNorm = rnorm(1), massPoint  = rnorm(1))))
  multiPointMassGen2 = combineGenerators(lapply(1:5, \(i)point_mass_feature_generator(muNorm = rnorm(1), massPoint  =  rnorm(1))))
  classGen = makeClassificationGenerator(list(multiPointMassGen1,multiPointMassGen2))
}

createDataSet(makePMClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'pointMassFeats5')

makePMClassProb =function(){
  multiPointMassGen1 = combineGenerators(lapply(1:10, \(i)point_mass_feature_generator(muNorm = rnorm(1), massPoint  = rnorm(1))))
  multiPointMassGen2 = combineGenerators(lapply(1:10, \(i)point_mass_feature_generator(muNorm = rnorm(1), massPoint  =  rnorm(1))))
  classGen = makeClassificationGenerator(list(multiPointMassGen1,multiPointMassGen2))
}

createDataSet(makePMClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'pointMassFeats10')

############################## Multiple Corr Patterns ##########################


makeMultiCorClassProb =function(){
  multiCorFeatGen1 = correlatedFeatureGenerator(ndim = 5,  rep(0,5), nCov= 6)
  multiCorFeatGen2 = correlatedFeatureGenerator(ndim = 5,  rep(0,5), nCov= 6)
  multiCorFeatGen3 = correlatedFeatureGenerator(ndim = 5,  rep(0,5), nCov= 6)
  multiCorFeatGen4 = correlatedFeatureGenerator(ndim = 5,  rep(0,5), nCov= 6)
  
  makeClassificationGenerator(
    list(
      multiCorFeatGen1,
      multiCorFeatGen2,
      multiCorFeatGen3,
      multiCorFeatGen4
    ), classLabels = c('c1', 'c1', 'c2', 'c2')
  )
}

createDataSet(makeMultiCorClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'multiCorFeats5')


makeMultiCorClassProb =function(){
  multiCorFeatGen1 = correlatedFeatureGenerator(ndim = 10,  rep(0,10), nCov= 11)
  multiCorFeatGen2 = correlatedFeatureGenerator(ndim = 10,  rep(0,10), nCov= 11)
  multiCorFeatGen3 = correlatedFeatureGenerator(ndim = 10,  rep(0,10), nCov= 11)
  multiCorFeatGen4 = correlatedFeatureGenerator(ndim = 10,  rep(0,10), nCov= 11)

  makeClassificationGenerator(
    list(
      multiCorFeatGen1,
      multiCorFeatGen2,
      multiCorFeatGen3,
      multiCorFeatGen4
    ), classLabels = c('c1', 'c1', 'c2', 'c2'))
}

createDataSet(makeMultiCorClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'multiCorFeats10')


################################ Multi Centric Cor ##############################

makeMultiCenterClassProb =function(){
  multiCorFeatGen1 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen2 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen3 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen4 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen5 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen6 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen7 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen8 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen9 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  multiCorFeatGen10 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 15)
  
  makeClassificationGenerator(
    list(
      multiCorFeatGen1,
      multiCorFeatGen2,
      multiCorFeatGen3,
      multiCorFeatGen4,
      multiCorFeatGen5,
      multiCorFeatGen6,
      multiCorFeatGen7,
      multiCorFeatGen8,
      multiCorFeatGen9,
      multiCorFeatGen10
    ), classLabels = c(rep('c1', 5), rep('c2', 5))
  )
}

createDataSet(makeMultiCenterClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'multiCenterFeats5')


makeMultiCenterClassProb =function(){
  multiCorFeatGen1 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen2 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen3 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen4 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen5 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen6 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen7 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen8 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen9 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 30)
  multiCorFeatGen10 = correlatedFeatureGenerator(ndim =10,  rnorm(10, 0, sd = .2), nCov= 30)
  
  makeClassificationGenerator(
    list(
      multiCorFeatGen1,
      multiCorFeatGen2,
      multiCorFeatGen3,
      multiCorFeatGen4,
      multiCorFeatGen5,
      multiCorFeatGen6,
      multiCorFeatGen7,
      multiCorFeatGen8,
      multiCorFeatGen9,
      multiCorFeatGen10
    ), classLabels = c(rep('c1', 5), rep('c2', 5))
  )
}

createDataSet(makeMultiCenterClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'multiCenterFeats10')

################################# Multi Class ##################################

makeMultiCenterClassProb =function(){
  multiCorFeatGen1 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen2 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen3 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen4 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen5 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen6 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen7 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen8 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen9 = correlatedFeatureGenerator(ndim = 5,  rnorm(5, 0, sd = .2), nCov= 6)
  multiCorFeatGen10 = correlatedFeatureGenerator(ndim = 5, rnorm(5, 0, sd = .2), nCov= 6)
  
  makeClassificationGenerator(
    list(
      multiCorFeatGen1,
      multiCorFeatGen2,
      multiCorFeatGen3,
      multiCorFeatGen4,
      multiCorFeatGen5,
      multiCorFeatGen6,
      multiCorFeatGen7,
      multiCorFeatGen8,
      multiCorFeatGen9,
      multiCorFeatGen10
    ), classLabels = paste0('c', 1:10) 
  )
}

createDataSet(makeMultiCenterClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'multiClassFeats5')


makeMultiCenterClassProb =function(){
  multiCorFeatGen1 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen2 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen3 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen4 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen5 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen6 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen7 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen8 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen9 = correlatedFeatureGenerator(ndim = 10,  rnorm(10, 0, sd = .2), nCov= 11)
  multiCorFeatGen10 = correlatedFeatureGenerator(ndim =10,  rnorm(10, 0, sd = .2), nCov= 11)
  
  makeClassificationGenerator(
    list(
      multiCorFeatGen1,
      multiCorFeatGen2,
      multiCorFeatGen3,
      multiCorFeatGen4,
      multiCorFeatGen5,
      multiCorFeatGen6,
      multiCorFeatGen7,
      multiCorFeatGen8,
      multiCorFeatGen9,
      multiCorFeatGen10
    ), classLabels = paste0('c', 1:10) 
  )
}

createDataSet(makeMultiCenterClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'multiClassFeats10')

############################# Make XOR Problems ################################

makeXORClassProb =function(){
  makeXORClassificationGenerator(makeUnifGenerator(5, limits = c(-1,1)))
}
createDataSet(makeXORClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'XOR5')

makeXORClassProb =function(){
  makeXORClassificationGenerator(makeUnifGenerator(10, limits = c(-1,1)))
}
createDataSet(makeXORClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'XOR10')

################################################################################


makeOneHotClassProb =function(){
  oneHotGen1 = combineGenerators(lapply(1:5, \(i)makeOneHotEncFeats()))
  oneHotGen2 = combineGenerators(lapply(1:5, \(i)makeOneHotEncFeats()))
  classGen = makeClassificationGenerator(list(oneHotGen1,oneHotGen2))
}

createDataSet(makeOneHotClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'oneHotEnc5')


makeOneHotClassProb =function(){
  oneHotGen1 = combineGenerators(lapply(1:10, \(i)makeOneHotEncFeats()))
  oneHotGen2 = combineGenerators(lapply(1:10, \(i)makeOneHotEncFeats()))
  classGen = makeClassificationGenerator(list(oneHotGen1,oneHotGen2))
}

createDataSet(makeOneHotClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'oneHotEnc10')

########################### SPARSE LOGIT#######################################

makeLogitClassProb = function(){
  multiCorFeatGen1 = combineGenerators(list(makeNoiseGenerator(ndim = 20, ynoise = 2)))

  makeLogitClassificationGenerator(multiCorFeatGen1, features = c(1, 5))
}
createDataSet(makeLogitClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'logit20')

############################# make circular data ###############################


makeCircClassProb =function(){
  makeCircularClassification(5)
}
createDataSet(makeCircClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'Circ5')

makeCircClassProb =function(){
  makeCircularClassification(10)
}
createDataSet(makeCircClassProb, nSets=20, nTrain=2000, nTest=2000, name = 'Circ10')

################################################################################



