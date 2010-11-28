unit UMIMEType;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils;

function GetMIMEType(FileExtension: string): string;

var
  MIMETypeList: TStringList;

implementation

function GetMIMEType(FileExtension: string): string;
begin
  Result := MIMETypeList.Values[FileExtension];
  if Result = '' then Result := 'application/octet-stream';
end;

initialization

MIMETypeList := TStringList.Create;
with MIMETypeList do begin
  Values['html'] := 'text/html';
  Values['png'] := 'image/png';
  Values['htm'] := 'text/html';
  Values['txt'] := 'text/text';
  Values['xml'] := 'text/xml';
  Values['jpeg'] := 'image/jpeg';
  Values['jpg'] := 'image/jpeg';
  Values['bmp'] := 'image/bmp';
  Values['css'] := 'text/css';
end;

finalization

MIMETypeList.Destroy;

end.

(*
  Add('3dm', 'x-world/x-3dmf');
  Add('3dmf', 'x-world/x-3dmf');
  Add('a  application/octet-stream');
  Add('aab  application/x-authorware-bin
  Add('aam  application/x-authorware-map');
  Add('aas  application/x-authorware-seg');
  Add('abc  text/vnd.abc');
  Add('acgi   text/html');
  Add('afl  video/animaflex');
  Add('ai   application/postscript');
  Add('aif  audio/aiff');
  Add('aif  audio/x-aiff');
  Add('aifc   audio/aiff');
  Add('aifc   audio/x-aiff');
  Add('aiff   audio/aiff');
  Add('aiff   audio/x-aiff');
  Add('aim  application/x-aim');
  Add('aip  text/x-audiosoft-intra');
  Add('ani  application/x-navi-animation');
  Add('aos  application/x-nokia-9000-communicator-add-on-software');
  Add('aps  application/mime');
  Add('arc  application/octet-stream');
  Add('arj  application/arj');
  Add('arj  application/octet-stream');
  Add('art  image/x-jg');
  Add('asf  video/x-ms-asf');
  Add('asm  text/x-asm');
  Add('asp  text/asp');
  Add('asx  application/x-mplayer2');
  Add('asx  video/x-ms-asf');
  Add('asx  video/x-ms-asf-plugin');
  Add('au   audio/basic');
  Add('au   audio/x-au');
  Add('avi  application/x-troff-msvideo');
  Add('avi  video/avi');
  Add('avi  video/msvideo');
  Add('avi  video/x-msvideo');
  Add('avs  video/avs-video');
  Add('bcpio  application/x-bcpio');
  Add('bin  application/mac-binary');
  Add('bin  application/macbinary');
  Add('bin  application/octet-stream
  Add('bin  application/x-binary
  Add('bin  application/x-macbinary
  Add('bm   image/bmp
  Add('bmp  image/bmp
  Add('bmp  image/x-windows-bmp
  Add('boo  application/book
  Add('book   application/book
  Add('boz  application/x-bzip2
  Add('bsh  application/x-bsh
  Add('bz   application/x-bzip
  Add('bz2  application/x-bzip2
  Add('c  text/plain
  Add('c  text/x-c
  Add('c++  text/plain
  Add('cat  application/vnd  Add('ms-pki  Add('seccat
  Add('cc   text/plain
  Add('cc   text/x-c
  Add('ccad   application/clariscad
  Add('cco  application/x-cocoa
  Add('cdf  application/cdf
  Add('cdf  application/x-cdf
  Add('cdf  application/x-netcdf
  Add('cer  application/pkix-cert
  Add('cer  application/x-x509-ca-cert
  Add('cha  application/x-chat
  Add('chat   application/x-chat
  Add('class  application/java
  Add('class  application/java-byte-code
  Add('class  application/x-java-class
  Add('com  application/octet-stream
  Add('com  text/plain
  Add('conf   text/plain
  Add('cpio   application/x-cpio
  Add('cpp  text/x-c
  Add('cpt  application/mac-compactpro
  Add('cpt  application/x-compactpro
  Add('cpt  application/x-cpt
  Add('crl  application/pkcs-crl
  Add('crl  application/pkix-crl
  Add('crt  application/pkix-cert
  Add('crt  application/x-x509-ca-cert
  Add('crt  application/x-x509-user-cert
  Add('csh  application/x-csh
  Add('csh  text/x-script  Add('csh
  Add('css  application/x-pointplus
  Add('css  text/css
  Add('cxx  text/plain
  Add('dcr  application/x-director
  Add('deepv  application/x-deepv
  Add('def  text/plain
  Add('der  application/x-x509-ca-cert
  Add('dif  video/x-dv
  Add('dir  application/x-director
  Add('dl   video/dl
  Add('dl   video/x-dl
  Add('doc  application/msword
  Add('dot  application/msword
  Add('dp   application/commonground
  Add('drw  application/drafting
  Add('dump   application/octet-stream
  Add('dv   video/x-dv
  Add('dvi  application/x-dvi
  Add('dwf  drawing/x-dwf (old)
  Add('dwf  model/vnd  Add('dwf
  Add('dwg  application/acad
  Add('dwg  image/vnd  Add('dwg
  Add('dwg  image/x-dwg
  Add('dxf  application/dxf
  Add('dxf  image/vnd  Add('dwg
  Add('dxf  image/x-dwg
  Add('dxr  application/x-director
  Add('el   text/x-script  Add('elisp
  Add('elc  application/x-bytecode  Add('elisp (compiled elisp)
  Add('elc  application/x-elc
  Add('env  application/x-envoy
  Add('eps  application/postscript
  Add('es   application/x-esrehber
  Add('etx  text/x-setext
  Add('evy  application/envoy
  Add('evy  application/x-envoy
  Add('exe  application/octet-stream
  Add('f  text/plain
  Add('f  text/x-fortran
  Add('f77  text/x-fortran
  Add('f90  text/plain
  Add('f90  text/x-fortran
  Add('fdf  application/vnd  Add('fdf
  Add('fif  application/fractals
  Add('fif  image/fif
  Add('fli  video/fli
  Add('fli  video/x-fli
  Add('flo  image/florian
  Add('flx  text/vnd  Add('fmi  Add('flexstor
  Add('fmf  video/x-atomic3d-feature
  Add('for  text/plain
  Add('for  text/x-fortran
  Add('fpx  image/vnd  Add('fpx
  Add('fpx  image/vnd  Add('net-fpx
  Add('frl  application/freeloader
  Add('funk   audio/make
  Add('g  text/plain
  Add('g3   image/g3fax
  Add('gif  image/gif
  Add('gl   video/gl
  Add('gl   video/x-gl
  Add('gsd  audio/x-gsm
  Add('gsm  audio/x-gsm
  Add('gsp  application/x-gsp
  Add('gss  application/x-gss
  Add('gtar   application/x-gtar
  Add('gz   application/x-compressed
  Add('gz   application/x-gzip
  Add('gzip   application/x-gzip
  Add('gzip   multipart/x-gzip
  Add('h  text/plain
  Add('h  text/x-h
  Add('hdf  application/x-hdf
  Add('help   application/x-helpfile
  Add('hgl  application/vnd  Add('hp-hpgl
  Add('hh   text/plain
  Add('hh   text/x-h
  Add('hlb  text/x-script
  Add('hlp  application/hlp
  Add('hlp  application/x-helpfile
  Add('hlp  application/x-winhelp
  Add('hpg  application/vnd  Add('hp-hpgl
  Add('hpgl   application/vnd  Add('hp-hpgl
  Add('hqx  application/binhex
  Add('hqx  application/binhex4
  Add('hqx  application/mac-binhex
  Add('hqx  application/mac-binhex40
  Add('hqx  application/x-binhex40
  Add('hqx  application/x-mac-binhex40
  Add('hta  application/hta
  Add('htc  text/x-component
  Add('htm  text/html
  Add('html   text/html
  Add('htmls  text/html
  Add('htt  text/webviewhtml
  Add('htx  text/html
  Add('ice  x-conference/x-cooltalk
  Add('ico  image/x-icon
  Add('idc  text/plain
  Add('ief  image/ief
  Add('iefs   image/ief
  Add('iges   application/iges
  Add('iges   model/iges
  Add('igs  application/iges
  Add('igs  model/iges
  Add('ima  application/x-ima
  Add('imap   application/x-httpd-imap
  Add('inf  application/inf
  Add('ins  application/x-internett-signup
  Add('ip   application/x-ip2
  Add('isu  video/x-isvideo
  Add('it   audio/it
  Add('iv   application/x-inventor
  Add('ivr  i-world/i-vrml
  Add('ivy  application/x-livescreen
  Add('jam  audio/x-jam
  Add('jav  text/plain
  Add('jav  text/x-java-source
  Add('java   text/plain
  Add('java   text/x-java-source
  Add('jcm  application/x-java-commerce
  Add('jfif   image/jpeg
  Add('jfif   image/pjpeg
  Add('jfif-tbnl  image/jpeg
  Add('jpe  image/jpeg
  Add('jpe  image/pjpeg
  Add('jpeg   image/jpeg
  Add('jpeg   image/pjpeg
  Add('jpg  image/jpeg
  Add('jpg  image/pjpeg
  Add('jps  image/x-jps
  Add('js   application/x-javascript
  Add('jut  image/jutvision
  Add('kar  audio/midi
  Add('kar  music/x-karaoke
  Add('ksh  application/x-ksh
  Add('ksh  text/x-script  Add('ksh
  Add('la   audio/nspaudio
  Add('la   audio/x-nspaudio
  Add('lam  audio/x-liveaudio
  Add('latex  application/x-latex
  Add('lha  application/lha
  Add('lha  application/octet-stream
  Add('lha  application/x-lha
  Add('lhx  application/octet-stream
  Add('list   text/plain
  Add('lma  audio/nspaudio
  Add('lma  audio/x-nspaudio
  Add('log  text/plain
  Add('lsp  application/x-lisp
  Add('lsp  text/x-script  Add('lisp
  Add('lst  text/plain
  Add('lsx  text/x-la-asf
  Add('ltx  application/x-latex
  Add('lzh  application/octet-stream
  Add('lzh  application/x-lzh
  Add('lzx  application/lzx
  Add('lzx  application/octet-stream
  Add('lzx  application/x-lzx
  Add('m  text/plain
  Add('m  text/x-m
  Add('m1v  video/mpeg
  Add('m2a  audio/mpeg
  Add('m2v  video/mpeg
  Add('m3u  audio/x-mpequrl
  Add('man  application/x-troff-man
  Add('map  application/x-navimap
  Add('mar  text/plain
  Add('mbd  application/mbedlet
  Add('mc$  application/x-magic-cap-package-1  Add('0
  Add('mcd  application/mcad
  Add('mcd  application/x-mathcad
  Add('mcf  image/vasa
  Add('mcf  text/mcf
  Add('mcp  application/netmc
  Add('me   application/x-troff-me
  Add('mht  message/rfc822
  Add('mhtml  message/rfc822
  Add('mid  application/x-midi
  Add('mid  audio/midi
  Add('mid  audio/x-mid
  Add('mid  audio/x-midi
  Add('mid  music/crescendo
  Add('mid  x-music/x-midi
  Add('midi   application/x-midi
  Add('midi   audio/midi
  Add('midi   audio/x-mid
  Add('midi   audio/x-midi
  Add('midi   music/crescendo
  Add('midi   x-music/x-midi
  Add('mif  application/x-frame
  Add('mif  application/x-mif
  Add('mime   message/rfc822
  Add('mime   www/mime
  Add('mjf  audio/x-vnd  Add('audioexplosion  Add('mjuicemediafile
  Add('mjpg   video/x-motion-jpeg
  Add('mm   application/base64
  Add('mm   application/x-meme
  Add('mme  application/base64
  Add('mod  audio/mod
  Add('mod  audio/x-mod
  Add('moov   video/quicktime
  Add('mov  video/quicktime
  Add('movie  video/x-sgi-movie
  Add('mp2  audio/mpeg
  Add('mp2  audio/x-mpeg
  Add('mp2  video/mpeg
  Add('mp2  video/x-mpeg
  Add('mp2  video/x-mpeq2a
  Add('mp3  audio/mpeg3
  Add('mp3  audio/x-mpeg-3
  Add('mp3  video/mpeg
  Add('mp3  video/x-mpeg
  Add('mpa  audio/mpeg
  Add('mpa  video/mpeg
  Add('mpc  application/x-project
  Add('mpe  video/mpeg
  Add('mpeg   video/mpeg
  Add('mpg  audio/mpeg
  Add('mpg  video/mpeg
  Add('mpga   audio/mpeg
  Add('mpp  application/vnd  Add('ms-project
  Add('mpt  application/x-project
  Add('mpv  application/x-project
  Add('mpx  application/x-project
  Add('mrc  application/marc
  Add('ms   application/x-troff-ms
  Add('mv   video/x-sgi-movie
  Add('my   audio/make
  Add('mzz  application/x-vnd  Add('audioexplosion  Add('mzz
  Add('nap  image/naplps
  Add('naplps   image/naplps
  Add('nc   application/x-netcdf
  Add('ncm  application/vnd  Add('nokia  Add('configuration-message
  Add('nif  image/x-niff
  Add('niff   image/x-niff
  Add('nix  application/x-mix-transfer
  Add('nsc  application/x-conference
  Add('nvd  application/x-navidoc
  Add('o  application/octet-stream
  Add('oda  application/oda
  Add('omc  application/x-omc
  Add('omcd   application/x-omcdatamaker
  Add('omcr   application/x-omcregerator
  Add('p  text/x-pascal
  Add('p10  application/pkcs10
  Add('p10  application/x-pkcs10
  Add('p12  application/pkcs-12
  Add('p12  application/x-pkcs12
  Add('p7a  application/x-pkcs7-signature
  Add('p7c  application/pkcs7-mime
  Add('p7c  application/x-pkcs7-mime
  Add('p7m  application/pkcs7-mime
  Add('p7m  application/x-pkcs7-mime
  Add('p7r  application/x-pkcs7-certreqresp
  Add('p7s  application/pkcs7-signature
  Add('part   application/pro_eng
  Add('pas  text/pascal
  Add('pbm  image/x-portable-bitmap
  Add('pcl  application/vnd  Add('hp-pcl
  Add('pcl  application/x-pcl
  Add('pct  image/x-pict
  Add('pcx  image/x-pcx
  Add('pdb  chemical/x-pdb
  Add('pdf  application/pdf
  Add('pfunk  audio/make
  Add('pfunk  audio/make  Add('my  Add('funk
  Add('pgm  image/x-portable-graymap
  Add('pgm  image/x-portable-greymap
  Add('pic  image/pict
  Add('pict   image/pict
  Add('pkg  application/x-newton-compatible-pkg
  Add('pko  application/vnd  Add('ms-pki  Add('pko
  Add('pl   text/plain
  Add('pl   text/x-script  Add('perl
  Add('plx  application/x-pixclscript
  Add('pm   image/x-xpixmap
  Add('pm   text/x-script  Add('perl-module
  Add('pm4  application/x-pagemaker
  Add('pm5  application/x-pagemaker
  Add('png  image/png
  Add('pnm  application/x-portable-anymap
  Add('pnm  image/x-portable-anymap
  Add('pot  application/mspowerpoint
  Add('pot  application/vnd  Add('ms-powerpoint
  Add('pov  model/x-pov
  Add('ppa  application/vnd  Add('ms-powerpoint
  Add('ppm  image/x-portable-pixmap
  Add('pps  application/mspowerpoint
  Add('pps  application/vnd  Add('ms-powerpoint
  Add('ppt  application/mspowerpoint
  Add('ppt  application/powerpoint
  Add('ppt  application/vnd  Add('ms-powerpoint
  Add('ppt  application/x-mspowerpoint
  Add('ppz  application/mspowerpoint
  Add('pre  application/x-freelance
  Add('prt  application/pro_eng
  Add('ps   application/postscript
  Add('psd  application/octet-stream
  Add('pvu  paleovu/x-pv
  Add('pwz  application/vnd  Add('ms-powerpoint
  Add('py   text/x-script  Add('phyton
  Add('pyc  applicaiton/x-bytecode  Add('python
  Add('qcp  audio/vnd  Add('qcelp
  Add('qd3  x-world/x-3dmf
  Add('qd3d   x-world/x-3dmf
  Add('qif  image/x-quicktime
  Add('qt   video/quicktime
  Add('qtc  video/x-qtc
  Add('qti  image/x-quicktime
  Add('qtif   image/x-quicktime
  Add('ra   audio/x-pn-realaudio
  Add('ra   audio/x-pn-realaudio-plugin
  Add('ra   audio/x-realaudio
  Add('ram  audio/x-pn-realaudio
  Add('ras  application/x-cmu-raster
  Add('ras  image/cmu-raster
  Add('ras  image/x-cmu-raster
  Add('rast   image/cmu-raster
  Add('rexx   text/x-script  Add('rexx
  Add('rf   image/vnd  Add('rn-realflash
  Add('rgb  image/x-rgb
  Add('rm   application/vnd  Add('rn-realmedia
  Add('rm   audio/x-pn-realaudio
  Add('rmi  audio/mid
  Add('rmm  audio/x-pn-realaudio
  Add('rmp  audio/x-pn-realaudio
  Add('rmp  audio/x-pn-realaudio-plugin
  Add('rng  application/ringing-tones
  Add('rng  application/vnd  Add('nokia  Add('ringing-tone
  Add('rnx  application/vnd  Add('rn-realplayer
  Add('roff   application/x-troff
  Add('rp   image/vnd  Add('rn-realpix
  Add('rpm  audio/x-pn-realaudio-plugin
  Add('rt   text/richtext
  Add('rt   text/vnd  Add('rn-realtext
  Add('rtf  application/rtf
  Add('rtf  application/x-rtf
  Add('rtf  text/richtext
  Add('rtx  application/rtf
  Add('rtx  text/richtext
  Add('rv   video/vnd  Add('rn-realvideo
  Add('s  text/x-asm
  Add('s3m  audio/s3m
  Add('saveme   application/octet-stream
  Add('sbk  application/x-tbook
  Add('scm  application/x-lotusscreencam
  Add('scm  text/x-script  Add('guile
  Add('scm  text/x-script  Add('scheme
  Add('scm  video/x-scm
  Add('sdml   text/plain
  Add('sdp  application/sdp
  Add('sdp  application/x-sdp
  Add('sdr  application/sounder
  Add('sea  application/sea
  Add('sea  application/x-sea
  Add('set  application/set
  Add('sgm  text/sgml
  Add('sgm  text/x-sgml
  Add('sgml   text/sgml
  Add('sgml   text/x-sgml
  Add('sh   application/x-bsh
  Add('sh   application/x-sh
  Add('sh   application/x-shar
  Add('sh   text/x-script  Add('sh
  Add('shar   application/x-bsh
  Add('shar   application/x-shar
  Add('shtml  text/html
  Add('shtml  text/x-server-parsed-html
  Add('sid  audio/x-psid
  Add('sit  application/x-sit
  Add('sit  application/x-stuffit
  Add('skd  application/x-koan
  Add('skm  application/x-koan
  Add('skp  application/x-koan
  Add('skt  application/x-koan
  Add('sl   application/x-seelogo
  Add('smi  application/smil
  Add('smil   application/smil
  Add('snd  audio/basic
  Add('snd  audio/x-adpcm
  Add('sol  application/solids
  Add('spc  application/x-pkcs7-certificates
  Add('spc  text/x-speech
  Add('spl  application/futuresplash
  Add('spr  application/x-sprite
  Add('sprite   application/x-sprite
  Add('src  application/x-wais-source
  Add('ssi  text/x-server-parsed-html
  Add('ssm  application/streamingmedia
  Add('sst  application/vnd  Add('ms-pki  Add('certstore
  Add('step   application/step
  Add('stl  application/sla
  Add('stl  application/vnd  Add('ms-pki  Add('stl
  Add('stl  application/x-navistyle
  Add('stp  application/step
  Add('sv4cpio  application/x-sv4cpio
  Add('sv4crc   application/x-sv4crc
  Add('svf  image/vnd  Add('dwg
  Add('svf  image/x-dwg
  Add('svr  application/x-world
  Add('svr  x-world/x-svr
  Add('swf  application/x-shockwave-flash
  Add('t  application/x-troff
  Add('talk   text/x-speech
  Add('tar  application/x-tar
  Add('tbk  application/toolbook
  Add('tbk  application/x-tbook
  Add('tcl  application/x-tcl
  Add('tcl  text/x-script  Add('tcl
  Add('tcsh   text/x-script  Add('tcsh
  Add('tex  application/x-tex
  Add('texi   application/x-texinfo
  Add('texinfo  application/x-texinfo
  Add('text   application/plain
  Add('text   text/plain
  Add('tgz  application/gnutar
  Add('tgz  application/x-compressed
  Add('tif  image/tiff
  Add('tif  image/x-tiff
  Add('tiff   image/tiff
  Add('tiff   image/x-tiff
  Add('tr   application/x-troff
  Add('tsi  audio/tsp-audio
  Add('tsp  application/dsptype
  Add('tsp  audio/tsplayer
  Add('tsv  text/tab-separated-values
  Add('turbot   image/florian
  Add('txt  text/plain
  Add('uil  text/x-uil
  Add('uni  text/uri-list
  Add('unis   text/uri-list
  Add('unv  application/i-deas
  Add('uri  text/uri-list
  Add('uris   text/uri-list
  Add('ustar  application/x-ustar
  Add('ustar  multipart/x-ustar
  Add('uu   application/octet-stream
  Add('uu   text/x-uuencode
  Add('uue  text/x-uuencode
  Add('vcd  application/x-cdlink
  Add('vcs  text/x-vcalendar
  Add('vda  application/vda
  Add('vdo  video/vdo
  Add('vew  application/groupwise
  Add('viv  video/vivo
  Add('viv  video/vnd  Add('vivo
  Add('vivo   video/vivo
  Add('vivo   video/vnd  Add('vivo
  Add('vmd  application/vocaltec-media-desc
  Add('vmf  application/vocaltec-media-file
  Add('voc  audio/voc
  Add('voc  audio/x-voc
  Add('vos  video/vosaic
  Add('vox  audio/voxware
  Add('vqe  audio/x-twinvq-plugin
  Add('vqf  audio/x-twinvq
  Add('vql  audio/x-twinvq-plugin
  Add('vrml   application/x-vrml
  Add('vrml   model/vrml
  Add('vrml   x-world/x-vrml
  Add('vrt  x-world/x-vrt
  Add('vsd  application/x-visio
  Add('vst  application/x-visio
  Add('vsw  application/x-visio
  Add('w60  application/wordperfect6  Add('0
  Add('w61  application/wordperfect6  Add('1
  Add('w6w  application/msword
  Add('wav  audio/wav
  Add('wav  audio/x-wav
  Add('wb1  application/x-qpro
  Add('wbmp   image/vnd  Add('wap  Add('wbmp
  Add('web  application/vnd  Add('xara
  Add('wiz  application/msword
  Add('wk1  application/x-123
  Add('wmf  windows/metafile
  Add('wml  text/vnd  Add('wap  Add('wml
  Add('wmlc   application/vnd  Add('wap  Add('wmlc
  Add('wmls   text/vnd  Add('wap  Add('wmlscript
  Add('wmlsc  application/vnd  Add('wap  Add('wmlscriptc
  Add('word   application/msword
  Add('wp   application/wordperfect
  Add('wp5  application/wordperfect
  Add('wp5  application/wordperfect6  Add('0
  Add('wp6  application/wordperfect
  Add('wpd  application/wordperfect
  Add('wpd  application/x-wpwin
  Add('wq1  application/x-lotus
  Add('wri  application/mswrite
  Add('wri  application/x-wri
  Add('wrl  application/x-world
  Add('wrl  model/vrml
  Add('wrl  x-world/x-vrml
  Add('wrz  model/vrml
  Add('wrz  x-world/x-vrml
  Add('wsc  text/scriplet
  Add('wsrc   application/x-wais-source
  Add('wtk  application/x-wintalk
  Add('xbm  image/x-xbitmap
  Add('xbm  image/x-xbm
  Add('xbm  image/xbm
  Add('xdr  video/x-amt-demorun
  Add('xgz  xgl/drawing
  Add('xif  image/vnd  Add('xiff
  Add('xl   application/excel
  Add('xla  application/excel
  Add('xla  application/x-excel
  Add('xla  application/x-msexcel
  Add('xlb  application/excel
  Add('xlb  application/vnd  Add('ms-excel
  Add('xlb  application/x-excel
  Add('xlc  application/excel
  Add('xlc  application/vnd  Add('ms-excel
  Add('xlc  application/x-excel
  Add('xld  application/excel
  Add('xld  application/x-excel
  Add('xlk  application/excel
  Add('xlk  application/x-excel
  Add('xll  application/excel
  Add('xll  application/vnd  Add('ms-excel
  Add('xll  application/x-excel
  Add('xlm  application/excel
  Add('xlm  application/vnd  Add('ms-excel
  Add('xlm  application/x-excel
  Add('xls  application/excel
  Add('xls  application/vnd  Add('ms-excel
  Add('xls  application/x-excel
  Add('xls  application/x-msexcel
  Add('xlt  application/excel');
  Add('xlt  application/x-excel');
  Add('xlv  application/x-excel');
  Add('xlw  application/excel');
  Add('xlw  application/vnd.ms-excel');
  Add('xlw  application/x-excel');
  Add('xlw  application/x-msexcel');
  Add('xm   audio/xm');
  Add('xml  application/xml');
  Add('xml  text/xml');
  Add('xmz  xgl/movie');
  Add('xpix   application/x-vnd.ls-xpix');
  Add('xpm  image/x-xpixmap');
  Add('xpm  image/xpm');
  Add('x-png  image/png');
  Add('xsr  video/x-amt-showrun');
  Add('xwd  image/x-xwd');
  Add('xwd  image/x-xwindowdump');
  Add('xyz  chemical/x-pdb');
  Add('z  application/x-compress');
  Add('z  application/x-compressed');
  Add('zip  application/x-compressed');
  Add('zip  application/x-zip-compressed');
  Add('zip  application/zip');
  Add('zip  multipart/x-zip');
  Add('zoo  application/octet-stream');
  Add('zsh  text/x-script.zsh');
end;
*)
