
BEGIN { fileno = 0; size = 0; } {
    print $0 > "./outdir/out_" fileno;
    size += length();
}

#-- Split into 10 Mb blocks
#size > (10000000) && /\/\// {
#-- Split into 1 Mb blocks
#size > (1000000) && /\/\// {
#-- Split into 200 kb blocks
size > (200000) && /\/\// {
    fileno++;
    size = 0;
}



