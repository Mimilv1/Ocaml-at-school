let f : out_channel = open_out "./BJR" in (output_string f "Bonjour le monde!");
close_out f;;