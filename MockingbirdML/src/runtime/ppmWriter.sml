structure PpmWriter :> PPMWRITER = 
struct

exception SizeMismatch

(* colors are expected to be in the range 0-255 *)
type color = int * int * int
type picture = {width : int, height : int, buffer : color Vector.vector}

fun writePicture loc picture =
    let
        val {width = width, height = height, buffer = buf} = picture
        val outFile = TextIO.openOut loc
        fun writeColor n s =
        (
            TextIO.output(outFile, Int.toString n);
            TextIO.output(outFile, s)
        )
        fun writePixel (i, (r, g, b)) =
        (
            writeColor r " ";
            writeColor g " ";
            writeColor b " ";
            ()
        )
    in
    (
        if width * height <> Vector.length(buf) then raise SizeMismatch
        else
        (
            TextIO.output(outFile, "P3\n");
            writeColor width " ";
            writeColor height "\n";
            TextIO.output(outFile, "255\n");
            (* appi maps effects over a vector *)
            Vector.appi writePixel buf;
            TextIO.closeOut outFile
        )
    )
    end

fun makeTestFile f =
    let 
        val w = 255
        val h = 300
        val v = Vector.concat (List.tabulate(h, fn i =>
                Vector.tabulate(w, fn i => (i, 0, 255 - i))))
        val p = {width = w, height = h, buffer = v}
    in
        writePicture f p
    end

end 
