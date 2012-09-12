function switchMapImages(img, filename1, filename2)
{
    if(img.src.indexOf(filename1) == -1)
	img.src = filename1;
    else
	img.src = filename2;
}
