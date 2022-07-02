BASIC.exe: BASICLexer.fs BASICParser.fs BASICAST.fs BASICUtils.fs BASICSemantic.fs BASICCodeGenerator.fs BASICMain.fs basiclib.dll
	fsc --nologo -r FSharp.PowerPack.dll BASICAST.fs BASICParser.fs BASICLexer.fs BASICUtils.fs BASICSemantic.fs BASICCodeGenerator.fs BASICMain.fs --out:BASIC.exe

BASICLexer.fs: BASICLexer.fsl
	fslex BASICLexer.fsl --unicode
	
BASICParser.fs: BASICParser.fsy
	fsyacc BASICParser.fsy --module BASICParser

basiclib.dll: basiclib.cs
	dmcs /t:library basiclib.cs
	
clean:
	rm BASICLexer.fs BASICParser.fs BASICParser.fsi BASIC.exe basiclib.dll
