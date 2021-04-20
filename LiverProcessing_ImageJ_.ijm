// Dummy code for processing liver image files
// For Grass Rat sleep/sugar/photoperiod project
// Script author: Anusha Shankar
// Contact: nushiamme@gmail.com

// open("C:/Users/nushi/OneDrive - Cornell University/Anusha_personal/Fairbanks/Research/GrassRats/GrassRat_Liver_Analyses/Slide 14 from cassette 1-Region 001.tif");
function processFolder(input) {
	list = getFileList(input);
	for (i=0; i<list.length; i++){
		if(File.isDirectory(input + list[i))
		processFolder("" + input + list[i]);
		if(endsWith(list[i], suffix))
		processFile(input, output, list[i]);
	}
}

function processFile(input, output, file) {
	run("Split Channels");
	selectWindow("Slide 14 from cassette 1-Region 001.tif (blue)");
	close();
	selectWindow("Slide 14 from cassette 1-Region 001.tif (red)");
	close();
	selectWindow("Slide 14 from cassette 1-Region 001.tif (green)");
	// saveAs("Tiff", ...);
	setAutoThreshold("Default dark"); // this is usually around (108, 255)
	run("Convert to Mask");
	run("Close");
	// Up to Slide 1 Casette 2
	// run("Analyze Particles...", "size=0.005-1 circularity=0.50-1.00 show=Outlines display clear summarize");
	// To to Slide 2 Casette 2
	// run("Analyze Particles...", "size=0.005-1 circularity=0.50-1.00 show=Outlines display clear summarize");
	// For Slide 3 Casette 1 onwards
	run("Analyze Particles...", "size=0.005-1 circularity=0.50-1.00 show=Outlines display clear summarize");
	// Size (in^2): 0.01-1 (0.01-Infinity for up to Slide 1 Casette 2; then this for Slide 2 Casette 2)
	// Size (in^2): 0.005-1 (From 
	saveAs("Tiff",...);
}	
	