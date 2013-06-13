signature PPMWRITER = 
sig
    
    (* type color = Word8.word * Word8.word * Word8.word *)
    type color = int * int * int
    type picture = {width : int, height : int, buffer : color Vector.vector}

    val writePicture : string -> picture -> unit
    val makeTestFile : string -> unit

end 
