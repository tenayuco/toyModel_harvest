#tomamos df temp que YA TIUENE LOS GRUPOS de ADJACENY y sobre eso centroides
#y sobre eso el histograma
#no sirve tanto
#porque en realidad, el argumento clave, es que
#en total se alcanzan más plantas
#que estaran en el nuevos reds

DF_TEMP_CEN <- DF_TEMP %>%
  group_by(Rep, numPlants, NUEVA_RUST, porcionCosecha, cluster)%>%
  summarise(X_mean = mean(X), Y_mean = mean(Y))


N_CEN <- dim(DF_TEMP_CEN)[1]

distance_matrix_CEN <- matrix(0,nrow=N_CEN,ncol=N_CEN)
for( i in 1:N_CEN){
  for( j in 1:N_CEN ){
    distance_matrix_CEN[i,j] <- sqrt( (DF_TEMP_CEN$X_mean[i] - 
                                         DF_TEMP_CEN$X_mean[j])^2 + 
                                        (DF_TEMP_CEN$Y_mean[i] - DF_TEMP_CEN$Y_mean[j])^2 )
  }
}

mean(distance_matrix_CEN)
