Name:           lyskom-elisp-client
Summary:        LysKOM client for Emacs
Version:        #REDHATCLIENTVERSION#
Release:        1
Copyright:      GPL
Group:          Applications/Internet
Source:         http://www.ida.liu.se/~davby/lyskom-#CLIENTVERSION#.tar.gz
Vendor:         Lysator
Packager:       Kent Engström <kent@lysator.liu.se>
Requires:       emacs >= 20.1

%description
Emacs client for the LysKOM conference system developed at Lysator
Academic Computer Society (http://www.lysator.liu.se/). To be able to
start the LysKOM client conveniently, just add

(autoload 'lyskom "lyskom-elisp-client" "Start a LysKOM session." t)

to your ~/.emacs file. You may then start the client by invoking
M-x lyskom.
%prep
%setup -n lyskom-#CLIENTVERSION#
%build
make
%install
install -o root -g root -m 644 lyskom-#CLIENTVERSION#.el  /usr/share/emacs/site-lisp/lyskom.el
install -o root -g root -m 644 lyskom-#CLIENTVERSION#.elc /usr/share/emacs/site-lisp/lyskom.elc

%files
/usr/share/emacs/site-lisp/lyskom.el
/usr/share/emacs/site-lisp/lyskom.elc
%doc README
%doc COPYING
%doc NEWS-0.46
