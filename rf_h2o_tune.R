system.time(rf.h20.fold3.ji <- h2o.randomForest(x.indep, y.dep, train.h2o,
                 ntrees = 500, max_depth = 20, 
                 stopping_metric = "RMSE", 
                 mtries = -1))

h2o.performance(rf.h20.fold3.ji)

#check variable importance 
vars_rf_h2o <- h2o.varimp(rf.h20.fold3.ji)


#making predictions on unseen data

system.time(
  predict.rforest <- as.data.frame(h2o.predict(rf.h20.fold3.ji, test.h2o)))

a1 <- test_16$revenue-(predict.rforest$predict)
sqrt(sum(a1^2))

system.time(
  predictDMC_class<- as.data.frame(h2o.predict(rf.h20.fold3.ji, class.h20 )))

saveRDS(predictDMC_class,file= "rf_h2o_fold3_trial1_ji.rdd")


#Grid Search for Model Comparison

ntrees_opt <- c(20,500)
maxdepth_opt <- c(20:30)
hyper_parameters <- list(ntrees=ntrees_opt,
                         max_depth=maxdepth_opt)

grid.rf <- h2o.grid("randomForest", hyper_params = hyper_params,
                 y = y, x = x,
                 seed = 123,
                 training_frame = train.h2o,
                 nfolds=5)





fit.best <- h2o.getModel(model_id = best_id)
h2o.varimp(fit.best)


predictions.rf<-as.data.frame(h2o.predict(fit.best,test.h2o))






