

structure Examples =
struct
	open CommonSyntax
    	
    structure OrderExamples =
    struct
        open MbOrder
		
		fun chain3 (x,y,z) = Echain ((Echain (x,y)),z)
		fun chain4 (w,x,y,z) = Echain ((chain3 (w,x,y)),z)
	
		val alpha = Variable.newvar "alpha"
		
		val examples = [
		
		( "Naive Raytracer",
			chain3 (
				EbreakG (Tflat, Dlayer (OneG, Dflat)), 
				EbreakS (Tarray Tflat, OneS), 
				Emmr (SampCase, 
					Emmr (GeoCase, 
						Ehit
					)
				)
			)
		),
		
		( "Naive Rasterizer",
			chain3 (
				EbreakG (Tflat, Dlayer (OneG, Dflat)), 
				EbreakS (Tarray Tflat, OneS), 
				Emmr (GeoCase, 
					Emmr (SampCase, 
						Ehit
					)
				)
			)
		),
		
		( "Raytracer with Single Top-Level Check",
			chain3 (
				EbreakG (Tflat, Dbound (Dlayer (OneG, Dflat))), 
				EbreakS (Tbounded (Tarray Tflat), OneS), 
				Emmr (SampCase,
					Echain (
						EboundS (Tbounded (Tarray Tflat)),
						Etest ( 
							chain3 (
								EunboundS (Tbounded (Tarray Tflat), Tflat),
								EunboundG (Tarray Tflat, Tflat),
								Emmr (GeoCase, 
									Ehit
								)
							)
						)
					)
				)
			)
		),
		
		( "Raytracer with Assumed Branch of a BVH",
			chain3 (
				EbreakS (Tflat, OneS), 
				EbreakG (Tarray Tflat, Dlayer (TwoGP, Dbound Dflat)),
				Emmr (SampCase, 
					Echain (
						EboundS (Tarray (Tbounded Tflat)),
						Emmr (GeoCase, 
							Etest (
								chain4(
									EunboundG (Tflat, Tbounded Tflat), 
									EunboundS (Tflat, Tflat), 
									EbreakG (Tflat, Dlayer (OneG, Dflat)), 
									Emmr (GeoCase, 
										Ehit
									)
								)
							)
						)
					)
				)
			)
		),
		
		( "Recursive Geometry Decomposition for Each Ray",
			Echain (
				EbreakS (Tflat, OneS), 
				Emmr (SampCase, 
					Echain (
						EboundS Tflat,
						Efix (alpha, (GeoSamps (Tflat, Tbounded Tflat), Hits Tflat),
							Echain ( 
								EbreakG (Tbounded Tflat, Dbound Dflat), 
								Etest (
									Echain (
										EunboundG (Tflat, Tbounded Tflat) , 
										EsizeCase (GeoCase, 2, 
											Echain (
												EbreakG (Tbounded Tflat, Dlayer (TwoGP, Dflat)),
												Emmr (GeoCase, 
													Elabel alpha
												)
											), 
											Echain ( 
                                                EunboundS (Tflat, Tflat), 
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
		)
		]
    end
	
	fun orderToPoint ex = (Variable.reset (); 
				Normalize.translate ex (
				CheckOrder.inferType ex)) 
	
	fun orderToDeblock ex = SubstSingletons.substProgram (Deblock.translate (orderToPoint ex))
	fun orderToSML s ex = PrintMbSML.printToFile s (LetNormalize.normalize (PrimitivizeSML.translate (orderToDeblock ex)))
	
	fun compile i outfile = 
			let
				val ex = OrderExamples.examples
				val _ = print ("Compiling renderer " ^ (Int.toString i) ^ "/" ^ (Int.toString (length ex)) ^ "...")
				val source = (#2 (List.nth (ex,i-1)))
				val res = orderToSML outfile source
				val _ = print "success.\n"
			in
				()
			end		
			
	fun main (prog_name, args) =
			case args of
			  [a0,a1] => (
				case Int.fromString a0 of 
				  NONE => (print "Expect number for first arg."; OS.Process.failure) 
				| SOME i => 
					let
					in
						(compile i a1; OS.Process.success)
					end)
			| _ => (print "Expect two arguments. \n"; OS.Process.failure)
			
		
(*	fun orderToMbbc ex = PrintMBBC.printToScreen (Primitivize.translate (orderToDeblock ex)) *)
	
	
end
