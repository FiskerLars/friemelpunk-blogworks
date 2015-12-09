
# transforming nb-style-posts into hakyl style post contents
sed '/^---/,/^---/{/^.*:/h;s/^\([^:]*:\).*$/\1/;y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/;x;s/^[^:]*:\(.*\)$/\1/;H;g;s/\n//;P;d};s/^BODY:$//'
