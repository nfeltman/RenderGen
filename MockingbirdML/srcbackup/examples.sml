

structure Examples =
struct
	open CommonSyntax
    
    structure OrderExamples =
    struct
        open MbOrder
        open CommonSyntax.OrderTypes
		
		val alpha = Variable.newvar "alpha"
        val repeatWork = 
			Echain ( GeoSamps (Tflat, Tarray Tflat),
				EbreakS (Tflat, OneS), 
				Emmr (SampCase, 
					Echain ( GeoSamps (Tflat, Tbounded Tflat), 
						EboundS (Tflat, Single),
						Efix (alpha, (GeoSamps (Tflat, Tbounded Tflat), Hits Tflat),
							Echain ( GeoSamps (Tbounded Tflat, Tbounded Tflat), 
								EboundG (Tbounded Tflat, Single) , 
								Etest (
									Echain ( GeoSamps (Tflat, Tbounded Tflat), 
										EunboundG (Tbounded Tflat, Single) , 
										EsizeCase (GeoCase, 2, 
											Echain ( GeoSamps (Tarray Tflat, Tbounded Tflat), 
												EbreakG (Tbounded Tflat, Dlayer (TwoGP, Dflat)),
												Emmr (GeoCase, 
													Elabel alpha
												)
											), 
											Echain ( GeoSamps (Tflat, Tflat), 
                                                EunboundS (Tflat, Single), 
                                                Ehit
                                            )
										)
									)
								)
							)
						)
					)
				)
			)
		val naiveSamplesFirst = 
			Echain ( GeoSamps (Tflat, Tarray Tflat),
				EbreakS (Tflat, OneS), 
				Emmr (SampCase, 
					Echain ( GeoSamps (Tarray Tflat, Tflat),
						EbreakG (Tflat, Dlayer (OneG, Dflat)), 
						Emmr (GeoCase, 
							Ehit
						)
					)
				)
			)
    end
	
	fun orderToPoint ex = 
				Pointify.translate (
				Normalize.translate ex (
				CheckOrder.inferType ex))
	
	fun orderToDetuple ex = Detuple.translate (orderToPoint ex)
	fun orderToDeblock ex = Deblock.translate (orderToDetuple ex)
	fun orderToMbbc ex = PrintMBBC.printToScreen (Primitivize.translate (orderToDeblock ex))
	
	
end
