Name: lyskom-elisp-client
Summary: The LysKOM Emacs Lisp Client.
Version: #CLIENTVERSION#
Release: 1
Requires: emacs >= 20.7
BuildRequires: emacs >= 20.7
BuildArchitectures: noarch
URL: http://www.lysator.liu.se/lyskom/
Source0: lyskom-elisp-client-src-%{version}.tar.gz
License: GPL
Group: Applications/Internet
Prefix: /usr
BuildRoot: %{_tmppath}/%{name}-root
Packager: C C Magnus Gustavsson <URL:http://magnus.gustavsson.se/>

%description
LysKOM is a project in progress at the Lysator Academic Computing
Society at Linköping University in Sweden. For information about
Lysator, see <URL:http://www.lysator.liu.se/>. For more information
on LysKOM, see <URL:http://www.lysator.liu.se/lyskom/>.

%prep
%setup -q -n lyskom-elisp-client-src-%{version}

%build
make

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_prefix}/share/emacs/site-lisp \
  $RPM_BUILD_ROOT%{_prefix}/share/emacs/site-lisp/site-start.d
install -m 644 lyskom-%{version}.elc \
  $RPM_BUILD_ROOT%{_prefix}/share/emacs/site-lisp/lyskom.elc
echo "(autoload 'lyskom \"lyskom\" \"Start LysKOM\" t)" > \
  $RPM_BUILD_ROOT%{_prefix}/share/emacs/site-lisp/site-start.d/lyskom-init.el

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc README COPYING NEWS-*

%{_prefix}/share/emacs/site-lisp/lyskom.elc
%{_prefix}/share/emacs/site-lisp/site-start.d/lyskom-init.el
