vol_qry <- function(id, from, to) {
  template <-
    '
    {
      trafficData(trafficRegistrationPointId: "<id>") {
        volume {
          byHour(from: "<from>", to: "<to>") {
            edges {
              node {
                from
                to
                total {
                  volumeNumbers {
                    volume
                  }
                }
              }
            }
          }
        }
      }
    }
    '
  
  return(glue(template, .open = "<", .close = ">"))
}