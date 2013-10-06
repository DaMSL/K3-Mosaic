
import java.io.FileInputStream;
import org.dbtoaster.dbtoasterlib.StreamAdaptor._;
import org.dbtoaster.dbtoasterlib.K3Collection._;
import org.dbtoaster.dbtoasterlib.Source._;
import org.dbtoaster.dbtoasterlib.dbtoasterExceptions._;

import scala.collection.mutable.Map;

package org.dbtoaster {
  object RST extends DBTQuery[SimpleVal[Double]] {
    val sources = new SourceMultiplexer(List(createInputStreamSource(new FileInputStream("data/r.dat"), List(createAdaptor("csv", "R", List(("fields", ","),("schema", "int,int"),("eventtype", "insert")))), Delimited("\n")),createInputStreamSource(new FileInputStream("data/s.dat"), List(createAdaptor("csv", "S", List(("fields", ","),("schema", "int,int"),("eventtype", "insert")))), Delimited("\n")),createInputStreamSource(new FileInputStream("data/t.dat"), List(createAdaptor("csv", "T", List(("fields", ","),("schema", "int,int"),("eventtype", "insert")))), Delimited("\n"))));

    var QUERY_1_1 = SimpleVal[Double](0.0);

    var QUERY_1_1_pT1 = new K3PersistentCollection[(Double), Double](Map(), None) /* out */;

    var QUERY_1_1_pS1 = new K3PersistentCollection[(Double), Double](Map(), None) /* out */;

    var QUERY_1_1_pR1 = new K3PersistentCollection[(Double), Double](Map(), None) /* out */;

    var QUERY_1_1_pR1_pT1 = new K3PersistentCollection[Tuple2[Double,Double], Double](Map(), Some(Map("1" -> SecondaryIndex[(Double),Tuple2[Double,Double], Double](x => x match {
      case Tuple2(x1,x2) => (x2) 
    }
    ),"0" -> SecondaryIndex[(Double),Tuple2[Double,Double], Double](x => x match {
      case Tuple2(x1,x2) => (x1) 
    }
    )))) /* out */;

    var QUERY_1_1_pR1_pS1 = new K3PersistentCollection[(Double), Double](Map(), None) /* out */;

    def dispatcher(event: Option[StreamEvent]): Unit = {
      def onInsertT(var_QUERY_1_1_pR1_pS1T_T__C: Double,var_QUERY_1_1_pR1_pS1T_D: Double): Unit = {
        if((QUERY_1_1_pT1).contains((var_QUERY_1_1_pR1_pS1T_T__C))) {
          QUERY_1_1.update((QUERY_1_1.get()) + (((QUERY_1_1_pT1).lookup((var_QUERY_1_1_pR1_pS1T_T__C), 0.0)) * (var_QUERY_1_1_pR1_pS1T_D))) 
        }
        else {
          QUERY_1_1.update({
            QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pS1T_T__C), 0.);

            (QUERY_1_1.get()) + ((0.) * (var_QUERY_1_1_pR1_pS1T_D))
          }
          ) 
        };

        ((x:K3IntermediateCollection[(Double), Double]) => {
          x match {
            case var___cse1 => {
              (var___cse1).foreach {
                (x:Tuple2[(Double), Double]) => {
                  x match {
                    case Tuple2((var_QUERY_1_1R_R__B),var_dv) => {
                      if((QUERY_1_1_pR1).contains((var_QUERY_1_1R_R__B))) {
                        QUERY_1_1_pR1.updateValue((var_QUERY_1_1R_R__B), ((QUERY_1_1_pR1).lookup((var_QUERY_1_1R_R__B), 0.0)) + (var_dv)) 
                      }
                      else {
                        QUERY_1_1_pR1.updateValue((var_QUERY_1_1R_R__B), var_dv) 
                      }
                    }
                  }
                }
              }
            }
          }
        }
        ).apply(QUERY_1_1_pR1_pT1.slice((var_QUERY_1_1_pR1_pS1T_T__C), List(1)).groupByAggregate(0., {
          (x:Tuple2[Tuple2[Double,Double], Double]) => {
            x match {
              case Tuple2(Tuple2(var_QUERY_1_1R_R__B,var___t1),var_v2) => {
                ((var_QUERY_1_1R_R__B))
              }
            }
          }
        }
        , {
          (x:Tuple2[(Tuple2[Double,Double]), Double]) => {
            x match {
              case Tuple2(Tuple2(var_QUERY_1_1R_R__B,var___t1),var_v2) => {
                (x:Double) => {
                  x match {
                    case var_accv_3 => {
                      ((var_QUERY_1_1_pR1_pS1T_D) * (var_v2)) + (var_accv_3)
                    }
                  }
                }
              }
            }
          }
        }
        ));

        if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pS1T_T__C))) {
          QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pS1T_T__C), ((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pS1T_T__C), 0.0)) + (var_QUERY_1_1_pR1_pS1T_D)) 
        }
        else {
          QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pS1T_T__C), var_QUERY_1_1_pR1_pS1T_D) 
        }
      };

      def onInsertS(var_QUERY_1_1_pR1_pT1S_S__B: Double,var_QUERY_1_1_pR1_pT1S_S__C: Double): Unit = {
        if((QUERY_1_1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__B))) {
          if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
            ((x:Double) => {
              x match {
                case var___cse3 => {
                  QUERY_1_1.update((QUERY_1_1.get()) + ((var___cse3) * ((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0))))
                }
              }
            }
            ).apply((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
          else {
            ((x:Double) => {
              x match {
                case var___cse3 => {
                  QUERY_1_1.update({
                    QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), 0.);

                    ((x:Double) => {
                      x match {
                        case var___cse1 => {
                          (QUERY_1_1.get()) + ((var___cse1) * (0.))
                        }
                      }
                    }
                    ).apply(var___cse3)
                  }
                  )
                }
              }
            }
            ).apply((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
        }
        else {
          if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
            QUERY_1_1.update({
              QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), 0.);

              ((x:Double) => {
                x match {
                  case var___cse2 => {
                    (QUERY_1_1.get()) + ((0.) * (var___cse2))
                  }
                }
              }
              ).apply((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0))
            }
            ) 
          }
          else {
            QUERY_1_1.update({
              QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), 0.);

              QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), 0.);

              (QUERY_1_1.get()) + (0.)
            }
            ) 
          }
        };

        if((QUERY_1_1_pT1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
          if((QUERY_1_1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__B))) {
            ((x:Double) => {
              x match {
                case var___cse2 => {
                  QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), (var___cse2) + ((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)))
                }
              }
            }
            ).apply((QUERY_1_1_pT1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) 
          }
          else {
            ((x:Double) => {
              x match {
                case var___cse2 => {
                  QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), {
                    QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), 0.);

                    ((x:Double) => {
                      x match {
                        case var___cse1 => {
                          var___cse1
                        }
                      }
                    }
                    ).apply(var___cse2)
                  }
                  )
                }
              }
            }
            ).apply((QUERY_1_1_pT1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) 
          }
        }
        else {
          if((QUERY_1_1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__B))) {
            QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), (QUERY_1_1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
          else {
            QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), {
              QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), 0.);

              0.
            }
            ) 
          }
        };

        if((QUERY_1_1_pR1).contains((var_QUERY_1_1_pR1_pT1S_S__B))) {
          if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
            ((x:Double) => {
              x match {
                case var___cse2 => {
                  QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), (var___cse2) + ((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)))
                }
              }
            }
            ).apply((QUERY_1_1_pR1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
          else {
            ((x:Double) => {
              x match {
                case var___cse2 => {
                  QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), {
                    QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), 0.);

                    ((x:Double) => {
                      x match {
                        case var___cse1 => {
                          var___cse1
                        }
                      }
                    }
                    ).apply(var___cse2)
                  }
                  )
                }
              }
            }
            ).apply((QUERY_1_1_pR1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
        }
        else {
          if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
            QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), (QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) 
          }
          else {
            QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), {
              QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), 0.);

              0.
            }
            ) 
          }
        };

        if((QUERY_1_1_pR1_pT1).contains(Tuple2(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C))) {
          QUERY_1_1_pR1_pT1.updateValue(Tuple2(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C), ((QUERY_1_1_pR1_pT1).lookup(Tuple2(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) + (1.)) 
        }
        else {
          QUERY_1_1_pR1_pT1.updateValue(Tuple2(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C), 1.) 
        }
      };

      def onInsertR(var_QUERY_1_1_pS1R_A: Double,var_QUERY_1_1_pS1R_R__B: Double): Unit = {
        if((QUERY_1_1_pR1).contains((var_QUERY_1_1_pS1R_R__B))) {
          QUERY_1_1.update((QUERY_1_1.get()) + ((var_QUERY_1_1_pS1R_A) * ((QUERY_1_1_pR1).lookup((var_QUERY_1_1_pS1R_R__B), 0.0)))) 
        }
        else {
          QUERY_1_1.update({
            QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pS1R_R__B), 0.);

            (QUERY_1_1.get()) + ((var_QUERY_1_1_pS1R_A) * (0.))
          }
          ) 
        };

        ((x:K3IntermediateCollection[(Double), Double]) => {
          x match {
            case var___cse1 => {
              (var___cse1).foreach {
                (x:Tuple2[(Double), Double]) => {
                  x match {
                    case Tuple2((var_QUERY_1_1T_T__C),var_dv) => {
                      if((QUERY_1_1_pT1).contains((var_QUERY_1_1T_T__C))) {
                        QUERY_1_1_pT1.updateValue((var_QUERY_1_1T_T__C), ((QUERY_1_1_pT1).lookup((var_QUERY_1_1T_T__C), 0.0)) + (var_dv)) 
                      }
                      else {
                        QUERY_1_1_pT1.updateValue((var_QUERY_1_1T_T__C), var_dv) 
                      }
                    }
                  }
                }
              }
            }
          }
        }
        ).apply(QUERY_1_1_pR1_pT1.slice((var_QUERY_1_1_pS1R_R__B), List(0)).groupByAggregate(0., {
          (x:Tuple2[Tuple2[Double,Double], Double]) => {
            x match {
              case Tuple2(Tuple2(var___t4,var_QUERY_1_1T_T__C),var_v2) => {
                ((var_QUERY_1_1T_T__C))
              }
            }
          }
        }
        , {
          (x:Tuple2[(Tuple2[Double,Double]), Double]) => {
            x match {
              case Tuple2(Tuple2(var___t4,var_QUERY_1_1T_T__C),var_v2) => {
                (x:Double) => {
                  x match {
                    case var_accv_10 => {
                      ((var_QUERY_1_1_pS1R_A) * (var_v2)) + (var_accv_10)
                    }
                  }
                }
              }
            }
          }
        }
        ));

        if((QUERY_1_1_pS1).contains((var_QUERY_1_1_pS1R_R__B))) {
          QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pS1R_R__B), ((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pS1R_R__B), 0.0)) + (var_QUERY_1_1_pS1R_A)) 
        }
        else {
          QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pS1R_R__B), var_QUERY_1_1_pS1R_A) 
        }
      };

      def onDeleteT(var_QUERY_1_1_pR1_pS1T_T__C: Double,var_QUERY_1_1_pR1_pS1T_D: Double): Unit = {
        if((QUERY_1_1_pT1).contains((var_QUERY_1_1_pR1_pS1T_T__C))) {
          QUERY_1_1.update((QUERY_1_1.get()) + (((QUERY_1_1_pT1).lookup((var_QUERY_1_1_pR1_pS1T_T__C), 0.0)) * ((var_QUERY_1_1_pR1_pS1T_D) * (-1.)))) 
        }
        else {
          QUERY_1_1.update({
            QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pS1T_T__C), 0.);

            (QUERY_1_1.get()) + ((0.) * ((var_QUERY_1_1_pR1_pS1T_D) * (-1.)))
          }
          ) 
        };

        ((x:K3IntermediateCollection[(Double), Double]) => {
          x match {
            case var___cse1 => {
              (var___cse1).foreach {
                (x:Tuple2[(Double), Double]) => {
                  x match {
                    case Tuple2((var_QUERY_1_1R_R__B),var_dv) => {
                      if((QUERY_1_1_pR1).contains((var_QUERY_1_1R_R__B))) {
                        QUERY_1_1_pR1.updateValue((var_QUERY_1_1R_R__B), ((QUERY_1_1_pR1).lookup((var_QUERY_1_1R_R__B), 0.0)) + (var_dv)) 
                      }
                      else {
                        QUERY_1_1_pR1.updateValue((var_QUERY_1_1R_R__B), var_dv) 
                      }
                    }
                  }
                }
              }
            }
          }
        }
        ).apply(QUERY_1_1_pR1_pT1.slice((var_QUERY_1_1_pR1_pS1T_T__C), List(1)).groupByAggregate(0., {
          (x:Tuple2[Tuple2[Double,Double], Double]) => {
            x match {
              case Tuple2(Tuple2(var_QUERY_1_1R_R__B,var___t7),var_v1) => {
                ((var_QUERY_1_1R_R__B))
              }
            }
          }
        }
        , {
          (x:Tuple2[(Tuple2[Double,Double]), Double]) => {
            x match {
              case Tuple2(Tuple2(var_QUERY_1_1R_R__B,var___t7),var_v1) => {
                (x:Double) => {
                  x match {
                    case var_accv_13 => {
                      ((var_QUERY_1_1_pR1_pS1T_D) * ((var_v1) * (-1.))) + (var_accv_13)
                    }
                  }
                }
              }
            }
          }
        }
        ));

        if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pS1T_T__C))) {
          QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pS1T_T__C), ((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pS1T_T__C), 0.0)) + ((var_QUERY_1_1_pR1_pS1T_D) * (-1.))) 
        }
        else {
          QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pS1T_T__C), (var_QUERY_1_1_pR1_pS1T_D) * (-1.)) 
        }
      };

      def onDeleteS(var_QUERY_1_1_pR1_pT1S_S__B: Double,var_QUERY_1_1_pR1_pT1S_S__C: Double): Unit = {
        if((QUERY_1_1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__B))) {
          if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
            ((x:Double) => {
              x match {
                case var___cse3 => {
                  QUERY_1_1.update((QUERY_1_1.get()) + ((var___cse3) * (((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) * (-1.))))
                }
              }
            }
            ).apply((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
          else {
            ((x:Double) => {
              x match {
                case var___cse3 => {
                  QUERY_1_1.update({
                    QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), 0.);

                    ((x:Double) => {
                      x match {
                        case var___cse1 => {
                          (QUERY_1_1.get()) + ((var___cse1) * (-0.))
                        }
                      }
                    }
                    ).apply(var___cse3)
                  }
                  )
                }
              }
            }
            ).apply((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
        }
        else {
          if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
            QUERY_1_1.update({
              QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), 0.);

              ((x:Double) => {
                x match {
                  case var___cse2 => {
                    (QUERY_1_1.get()) + ((0.) * ((var___cse2) * (-1.)))
                  }
                }
              }
              ).apply((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0))
            }
            ) 
          }
          else {
            QUERY_1_1.update({
              QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), 0.);

              QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), 0.);

              (QUERY_1_1.get()) + ((0.) * (-0.))
            }
            ) 
          }
        };

        if((QUERY_1_1_pT1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
          if((QUERY_1_1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__B))) {
            ((x:Double) => {
              x match {
                case var___cse2 => {
                  QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), (var___cse2) + (((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) * (-1.)))
                }
              }
            }
            ).apply((QUERY_1_1_pT1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) 
          }
          else {
            ((x:Double) => {
              x match {
                case var___cse2 => {
                  QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), {
                    QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), 0.);

                    ((x:Double) => {
                      x match {
                        case var___cse1 => {
                          (var___cse1) + (-0.)
                        }
                      }
                    }
                    ).apply(var___cse2)
                  }
                  )
                }
              }
            }
            ).apply((QUERY_1_1_pT1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) 
          }
        }
        else {
          if((QUERY_1_1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__B))) {
            QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), ((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) * (-1.)) 
          }
          else {
            QUERY_1_1_pT1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), {
              QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), 0.);

              (0.) * (-1.)
            }
            ) 
          }
        };

        if((QUERY_1_1_pR1).contains((var_QUERY_1_1_pR1_pT1S_S__B))) {
          if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
            ((x:Double) => {
              x match {
                case var___cse2 => {
                  QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), (var___cse2) + (((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) * (-1.)))
                }
              }
            }
            ).apply((QUERY_1_1_pR1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
          else {
            ((x:Double) => {
              x match {
                case var___cse2 => {
                  QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), {
                    QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), 0.);

                    ((x:Double) => {
                      x match {
                        case var___cse1 => {
                          (var___cse1) + (-0.)
                        }
                      }
                    }
                    ).apply(var___cse2)
                  }
                  )
                }
              }
            }
            ).apply((QUERY_1_1_pR1).lookup((var_QUERY_1_1_pR1_pT1S_S__B), 0.0)) 
          }
        }
        else {
          if((QUERY_1_1_pR1_pS1).contains((var_QUERY_1_1_pR1_pT1S_S__C))) {
            QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), ((QUERY_1_1_pR1_pS1).lookup((var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) * (-1.)) 
          }
          else {
            QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pR1_pT1S_S__B), {
              QUERY_1_1_pR1_pS1.updateValue((var_QUERY_1_1_pR1_pT1S_S__C), 0.);

              (0.) * (-1.)
            }
            ) 
          }
        };

        if((QUERY_1_1_pR1_pT1).contains(Tuple2(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C))) {
          QUERY_1_1_pR1_pT1.updateValue(Tuple2(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C), ((QUERY_1_1_pR1_pT1).lookup(Tuple2(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C), 0.0)) + (-1.)) 
        }
        else {
          QUERY_1_1_pR1_pT1.updateValue(Tuple2(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C), -1.) 
        }
      };

      def onDeleteR(var_QUERY_1_1_pS1R_A: Double,var_QUERY_1_1_pS1R_R__B: Double): Unit = {
        if((QUERY_1_1_pR1).contains((var_QUERY_1_1_pS1R_R__B))) {
          QUERY_1_1.update((QUERY_1_1.get()) + ((var_QUERY_1_1_pS1R_A) * (((QUERY_1_1_pR1).lookup((var_QUERY_1_1_pS1R_R__B), 0.0)) * (-1.)))) 
        }
        else {
          QUERY_1_1.update({
            QUERY_1_1_pR1.updateValue((var_QUERY_1_1_pS1R_R__B), 0.);

            (QUERY_1_1.get()) + ((var_QUERY_1_1_pS1R_A) * (-0.))
          }
          ) 
        };

        ((x:K3IntermediateCollection[(Double), Double]) => {
          x match {
            case var___cse1 => {
              (var___cse1).foreach {
                (x:Tuple2[(Double), Double]) => {
                  x match {
                    case Tuple2((var_QUERY_1_1T_T__C),var_dv) => {
                      if((QUERY_1_1_pT1).contains((var_QUERY_1_1T_T__C))) {
                        QUERY_1_1_pT1.updateValue((var_QUERY_1_1T_T__C), ((QUERY_1_1_pT1).lookup((var_QUERY_1_1T_T__C), 0.0)) + (var_dv)) 
                      }
                      else {
                        QUERY_1_1_pT1.updateValue((var_QUERY_1_1T_T__C), var_dv) 
                      }
                    }
                  }
                }
              }
            }
          }
        }
        ).apply(QUERY_1_1_pR1_pT1.slice((var_QUERY_1_1_pS1R_R__B), List(0)).groupByAggregate(0., {
          (x:Tuple2[Tuple2[Double,Double], Double]) => {
            x match {
              case Tuple2(Tuple2(var___t11,var_QUERY_1_1T_T__C),var_v1) => {
                ((var_QUERY_1_1T_T__C))
              }
            }
          }
        }
        , {
          (x:Tuple2[(Tuple2[Double,Double]), Double]) => {
            x match {
              case Tuple2(Tuple2(var___t11,var_QUERY_1_1T_T__C),var_v1) => {
                (x:Double) => {
                  x match {
                    case var_accv_20 => {
                      ((var_QUERY_1_1_pS1R_A) * ((var_v1) * (-1.))) + (var_accv_20)
                    }
                  }
                }
              }
            }
          }
        }
        ));

        if((QUERY_1_1_pS1).contains((var_QUERY_1_1_pS1R_R__B))) {
          QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pS1R_R__B), ((QUERY_1_1_pS1).lookup((var_QUERY_1_1_pS1R_R__B), 0.0)) + ((var_QUERY_1_1_pS1R_A) * (-1.))) 
        }
        else {
          QUERY_1_1_pS1.updateValue((var_QUERY_1_1_pS1R_R__B), (var_QUERY_1_1_pS1R_A) * (-1.)) 
        }
      };
      
      event match {
        case Some(StreamEvent(InsertTuple, o, "T", (var_QUERY_1_1_pR1_pS1T_T__C: Double) :: (var_QUERY_1_1_pR1_pS1T_D: Double) :: Nil)) => onInsertT(var_QUERY_1_1_pR1_pS1T_T__C,var_QUERY_1_1_pR1_pS1T_D);

        case Some(StreamEvent(InsertTuple, o, "S", (var_QUERY_1_1_pR1_pT1S_S__B: Double) :: (var_QUERY_1_1_pR1_pT1S_S__C: Double) :: Nil)) => onInsertS(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C);

        case Some(StreamEvent(InsertTuple, o, "R", (var_QUERY_1_1_pS1R_A: Double) :: (var_QUERY_1_1_pS1R_R__B: Double) :: Nil)) => onInsertR(var_QUERY_1_1_pS1R_A,var_QUERY_1_1_pS1R_R__B);

        case Some(StreamEvent(DeleteTuple, o, "T", (var_QUERY_1_1_pR1_pS1T_T__C: Double) :: (var_QUERY_1_1_pR1_pS1T_D: Double) :: Nil)) => onDeleteT(var_QUERY_1_1_pR1_pS1T_T__C,var_QUERY_1_1_pR1_pS1T_D);

        case Some(StreamEvent(DeleteTuple, o, "S", (var_QUERY_1_1_pR1_pT1S_S__B: Double) :: (var_QUERY_1_1_pR1_pT1S_S__C: Double) :: Nil)) => onDeleteS(var_QUERY_1_1_pR1_pT1S_S__B,var_QUERY_1_1_pR1_pT1S_S__C);

        case Some(StreamEvent(DeleteTuple, o, "R", (var_QUERY_1_1_pS1R_A: Double) :: (var_QUERY_1_1_pS1R_R__B: Double) :: Nil)) => onDeleteR(var_QUERY_1_1_pS1R_A,var_QUERY_1_1_pS1R_R__B);

        case None => ();

        case _ => throw ShouldNotHappenError("Event could not be dispatched: " + event)
      }
    }
    
    def getResult = QUERY_1_1
  }
}
