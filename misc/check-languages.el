BABYL OPTIONS:
Version: 5
Labels:svmud,new,df,news',daily,mews,nwes,kvittens,svenskmudid,face,uudecoded,ans,svmudid,news,n,hopp,core,nws,bounced,kvitterad
Note:   This is the header of an rmail file.
Note:   If you are seeing it in rmail,
Note:    it means the file has no messages in it.
Mail: /usr/spool/mail/linus,~/mbox,~/mail/OUTGOING

1,,
Summary-line:  9-Dec                         pw 116 #assoc bla
Received: by lave.lysator.liu.se (5.64/1.34)
          (5.64/1.34/Lysator-3.1) id AA02594; Mon, 9 Dec 91 01:47:48 +0100
          (rfc931-sender: pw@lave.lysator.liu.se)
Date: Mon, 9 Dec 91 01:47:48 +0100
From: pw (P{r Winzell)
Message-Id: <9112090047.AA02594@lave.lysator.liu.se>
To: linus
Subject: assoc bla

*** EOOH ***
Date: Mon, 9 Dec 91 01:47:48 +0100
From: pw (P{r Winzell)
To: linus
Subject: assoc bla



  Hej,

  H{r {r ungef{r vad jag lurat ut om alists.
  Detta {r vad som st}r i DONE :

New efun:
mixed assoc(mixed key, mixed *keys, mixed *|void data_or_fail, mixed|void fail);
Searches a key in an alist.

Three modes of calling:
   i ) With exactly two arguments, the second being an array which's first
       element is no array.
       In this case the entire array is searched for the key; -1 is returned
	 if not found, else the index ( like member_array, but faster ).

   ii) With two or three arguments, the second being an array which's first
       element is an array.
       The array has to have a second element of the same size;
       the key is searched in the first and the associated element of the
       second array that is element of second argument is returned if succesful;
       if not, 0 is returned, or the third argument, if given.

   iii) With three or four arguments, the second being an array of keys
	( first element no array ) and the second is a matching data array.
					/* ^^^^^^ ska vara third / PW */
	returns 0 or fourth argument ( if given ) for failure,
	or the matching entry in the array given as third argument for success.

Complexity : O( lg(n) ) , where n is the number of keys.

Return value is undefined if another list is given in place of a presorted
key list.


New efun:
mixed insert_alist( mixed key, mixed data_or_key_list..., mixed * alist);
inserts an entry into an alist, or shows the place where this is to be done.
When called with the last argument being an alist:
  The first argument is a key to be inserted, the second and all the
  following but the last are data to associate it with.
  The last has to be an array with as much elements as key and data arguments
  are given, the matching key and data arrays; this should be already an
  alist, or the return value will neither be an alist.
  Return value is the enlarged assoc list ( array of two arrays ).
  If the key is already in the list, the data is simply replaced
  in the returned list.

When called with the last argument beinig a list of non-lists:
  The call has to be done with exactly two arguments.
  The first argument is a key to be inserted in the presorted key list
  ( first element of an array that is an alist )
  that has to be given as second argument. Return value is the index
  where the key has to be inserted to preserve the structure of a presorted
  alist, or the index where the key has been found.
  Return value is an int.
  CEVEATS: when called with certain string keys, the correct place might
    change after the call. So better don't use this mode of calling with
    a string key.

Complexity O( lg(n) + a*n ) Where n is the number of keys and s is
a very small constant ( for block move );

/*
  ARRGH, Complexity O(n + lg(n)) ...
*/

New efun:
mixed *order_alist(mixed *keys, mixed *|void data, ...);
Creates an alist.
Either takes an array containing keys, and others containing the associated
data, where all arrays are to be of the same length,
or takes a single array that contains as first member the array of keys
and has an arbitrary number of other members containing data, each of wich
has to be of the same length as the key array.
Returns an array holding the sorted key array and the data arrays; the same
permutation that is applied to the key array is applied to all data arrays.

Complexity is O( n * lg(n) * m ) , where n is the number of elements in the key
array and m is the number of data arrays + 1;

/* Orkar inte ens kommentera ... */

Note that the the dimensions of the arrays are used the other way than in lisp
to allow for faster searching.

Keys have to be of type integer, string or object. Types can be mixed.


Tydligen {r strukturen denna : Du har en m{ngd arrayer av samma l{ngd.
En av dessa {r en "key list" vars element kan best} av objekt, str{ngar,
integers men inte andra arrayer. Den sorterar efter pointers, s} man
ska inte strula omkring och {ndra str{ngarna (}tminstonde inte utan att
sortera om den efter}t efterssom det {r shared strings och pekarna {ndras
n{r man {ndrar str{ngarna).

Till assoc skickar du alltid som f|rsta element en nyckel att s|ka p}.
Andra argumentet {r en array. [r det en array av nycklar fungerar assoc
som en snabb member_array, returnerar index eller -1. Best}r arrayen av
tv} sub-arrayer tar den den f|rsta, dvs arg2[0] som keylist och returnerar
det associerade elementet i arg2[1][index].

Jaja, det h{r kan du ju l{sa sj{lv... Vet man lite om det kan man deciphera
dessa urusla docs.

 / PW





1,,
Summary-line:  9-Jan         olof@olorin.hsh.se  28 #Rum och object i 3.0.
Received: by lysator.liu.se (5.64/1.34)
          (5.64/1.34/Lysator-3.1) id AA11000; Thu, 9 Jan 92 13:10:02 +0100 
          (rfc931-sender: svmud@lysator.liu.se)
Date: Thu, 9 Jan 92 13:10:02 +0100
Message-Id: <9201091210.AA11000@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: olof@olorin.hsh.se (Tulkas i Svenskmud)
Reply-To: olof@olorin.hsh.se (Tulkas i Svenskmud)
X-From-Mud-User: Tulkas i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Rum och object i 3.0.

*** EOOH ***
Date: Thu, 9 Jan 92 13:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: olof@olorin.hsh.se (Tulkas i Svenskmud)
Reply-To: olof@olorin.hsh.se (Tulkas i Svenskmud)
X-From-Mud-User: Tulkas i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Rum och object i 3.0.

Brev fr}n Tulkas i SvenskMUD.
Kopia till:   radagast

Hej igen!
Jag skulle vilja f} exemplifierat hur man enligt alla nya normer
skall skapa dels ett rum, dels en npc, smat slutligen ett vanligt
object, ta den klassiska rose (*smile*).
Detta f|r att med en g}ng kunna komma ig}ng med skrivandet av ny
kod.
Skickar detta brev {ven till Radagast f|r att se vem som kan svara
snarast.

Kanske man skulle ha dessa tre filer i ngt bibliotek f|r alla
att l{sa .......???

/Tulkas


1,,
Summary-line:  9-Apr          bertil@isy.liu.se  58 #Första utskick !!!
Received: from isy.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA12407; Thu, 9 Apr 1992 08:30:11 +0200 
Received: from saint.isy.liu.se by isy.liu.se (5.65b/isy.minimaster-V1.0b2)
	id AA24535; Thu, 9 Apr 92 08:30:08 +0200
Date: Thu, 9 Apr 92 08:30:08 +0200
From: Bertil Reinhammar <bertil@isy.liu.se>
Message-Id: <9204090630.AA24535@isy.liu.se>
To: distribution:@isy.liu.se; (see end of body)
Subject: Första utskick !!!

*** EOOH ***
Date: Thu, 9 Apr 92 08:30:08 +0200
From: Bertil Reinhammar <bertil@isy.liu.se>
To: distribution:@isy.liu.se; (see end of body)
Subject: Första utskick !!!

!!!
Detta är ett första brev till deltagarna som gav sin email-address
i samband med seminariet Fredag 3 April.

Finns det ett intresse att bilda en avnämare till LPF ( League for
Programming Freedom ) ? Jag kan ordna till en mailinglista.

De dokument jag fick av Michael Tiemann är nu:

Storlek		     Namn

 23850 Apr  3 11:12 borland-amicus-brief
 20588 Apr  3 11:12 gatt.text
  2718 Apr  3 11:12 integration.patent
 11818 Apr  3 11:11 kapor.testimony
 55568 Apr  3 11:17 laf-fallacies.dvi
134200 Apr  3 11:28 laf-fallacies.ps
 44847 Apr  3 11:13 laf-fallacies.texi
  3343 Apr  3 11:14 mit.announcement
 55672 Apr  3 11:09 patent-office.answer
 69020 Apr  3 11:17 patent-office.dvi
146454 Apr  3 11:28 patent-office.ps
 29151 Apr  3 11:10 patent-office.rfc
  5530 Apr  3 11:11 patent.4916610
  2501 Apr  3 11:12 patent.4956809
  9876 Apr  3 11:12 patent.5083262
  5502 Apr  3 11:11 supreme-court.patents

Som ni ser är inte alla lämpliga att skicka via mail. Jag placerar dem
på ftp @isy.liu.se. De som inte kan hämta, säg till.

Om intresse finns kan sannolikt mer material hämtas.

Mvh/
---BeRe
------------------------------------------------------------------------------
Bertil Reinhammar, Dep. of EE. Linkoping University, S-58183 Linkoping, SWEDEN
Phone: +46 13 282647
email: bertil@isy.liu.se


%%% overflow headers %%%
To: bellman@lysator.liu.se, bertil@isy.liu.se, brant@ida.liu.se,
        ceder@lysator.liu.se, davpa@ida.liu.se, garp@isy.liu.se,
        ingwa@isy.liu.se, j_hedbrant@udv.liu.se, jlo@ida.liu.se,
        jonas-m@isy.liu.se, jonas-y@isy.liu.se, linus@lysator.liu.se,
        marst@ida.liu.se, micja@ida.liu.se, micke@isy.liu.se, msa@ida.liu.se,
        nican@ida.liu.se, noppe@lysator.liu.se, paf@ida.liu.se,
        parca@ida.liu.se, pen@lysator.liu.se, roger@isy.liu.se,
        tegen@isy.liu.se, tomhv@li.icl.se, tommy@isy.liu.se, tommyp@sectra.se,
        willfor@lysator.liu.se
%%% end overflow headers %%%


1, answered,,
Summary-line: 21-Jul               david@ruc.dk  12 #Re: Rating of messages
Received: from gorm.ruc.dk by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA06334; Tue, 21 Jul 1992 00:12:10 +0200 
Received: by gorm.ruc.dk (4.1/JBA-1.18)
	id AA07269; Tue, 21 Jul 92 00:10:03 +0200
Date: Tue, 21 Jul 92 00:10:03 +0200
From: david@ruc.dk (David Stodolsky)
Message-Id: <9207202210.AA07269@gorm.ruc.dk>
To: linus@lysator.liu.se
Subject: Re: Rating of messages
Newsgroups: news.future,gnu.emacs.gnus
References: <1992Jul6.011605.13619@husc3.harvard.edu> <br1hr8.i1h@wang.com> <LINUS.92Jul10041243@lysator.lysator.liu.se>

*** EOOH ***
Date: Tue, 21 Jul 92 00:10:03 +0200
From: david@ruc.dk (David Stodolsky)
To: linus@lysator.liu.se
Subject: Re: Rating of messages
Newsgroups: news.future,gnu.emacs.gnus
References: <1992Jul6.011605.13619@husc3.harvard.edu> <br1hr8.i1h@wang.com> <LINUS.92Jul10041243@lysator.lysator.liu.se>

nice hack, but a better way to use the ratings is needed before people will
make the effort of giving them.

want to see my paper on this?


1,,
Summary-line: 21-Jul           to: david@ruc.dk  20 #Rating of messages
Date: Tue, 21 Jul 92 00:47:24 EST
From: linus
To: david@ruc.dk
In-reply-to: David Stodolsky's message of Tue, 21 Jul 92 00:10:03 +0200 <9207202210.AA07269@gorm.ruc.dk>
Subject: Rating of messages

*** EOOH ***
Date: Tue, 21 Jul 92 00:47:24 EST
From: linus
To: david@ruc.dk
In-reply-to: David Stodolsky's message of Tue, 21 Jul 92 00:10:03 +0200 <9207202210.AA07269@gorm.ruc.dk>
Subject: Rating of messages

What do you mean better way to use the ratings? Are there any better
ways than ordering the articles using the ratings?

I mean:

Yes, I would like to see your paper.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)



1,,
Summary-line: 20-Jul        sasha@ra.cs.umb.edu  41 #Re: Rating of messages
Received: from cs.umb.edu by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA07511; Tue, 21 Jul 1992 02:41:08 +0200 
          (rfc931-sender: root@cs.umb.edu)
Received: from ra.cs.umb.edu by cs.umb.edu with SMTP id AA15218
  (5.65c/IDA-1.4.4 for <linus@lysator.liu.se>); Mon, 20 Jul 1992 20:41:00 -0400
Received: by ra.cs.umb.edu (5.65c/1.34)
	id AA14049; Mon, 20 Jul 1992 20:40:59 -0400
Date: Mon, 20 Jul 1992 20:40:59 -0400
From: sasha@ra.cs.umb.edu (Alexander Chislenko)
Message-Id: <199207210040.AA14049@ra.cs.umb.edu>
To: linus@lysator.liu.se
Subject: Re: Rating of messages
Newsgroups: news.future,gnu.emacs.gnus
In-Reply-To: <LINUS.92Jul10041243@lysator.lysator.liu.se>
References: <1992Jul6.011605.13619@husc3.harvard.edu> <br1hr8.i1h@wang.com>
Organization: University of Massachusetts at Boston
Cc: sasha@ra.cs.umb.edu

*** EOOH ***
Date: Mon, 20 Jul 1992 20:40:59 -0400
From: sasha@ra.cs.umb.edu (Alexander Chislenko)
To: linus@lysator.liu.se
Subject: Re: Rating of messages
Newsgroups: news.future,gnu.emacs.gnus
In-Reply-To: <LINUS.92Jul10041243@lysator.lysator.liu.se>
References: <1992Jul6.011605.13619@husc3.harvard.edu> <br1hr8.i1h@wang.com>
Organization: University of Massachusetts at Boston
Cc: sasha@ra.cs.umb.edu

   Great! I did hope that my suggestion would bring some practical action, but
not that soon...
   Did you read my message (the one that started the "Rating" thread)?
   What do you think of its other ideas?
   Are you interested in expanding the project?  I have some ideas on
features, implementation details, organizational issues, etc., and
would like to discuss them if you are interested.
   I think that it is very important to discuss and resolve some issues
BEFORE the software gets out.  

   Also, I'd be happy if you mention me as a person who suggested the idea
and would like to share suggestions on its grand-scale extentions.
It's not only that I think I deserve some credit for having worked on
Internet semantic enhancements and putting some of them forward in an
[apparently] appealing form - it would improve my chances of being heard
with this and other suggestions, and have something finally done.
   I would also like to get some grants for a serious implementation of
this project, and it looks like you are an ideal candidate for it.
   So I suggest that we cooperate.

   With best wishes,
					Alex.

  P.S.  All weird sounding parts of this text are due to my poor English.
	All good parts (if any) are really mine.
-- 
------------------------------------------------------------------------------
|  Alexander Chislenko | sasha@cs.umb.edu | Cambridge, MA  |  (617) 864-3382 |
------------------------------------------------------------------------------



1,,
Summary-line: 27-Jul      asb@media-lab.mit.edu  26 #Re: One more cool pizza party/MUD discussion! 
Received: from media-lab.mit.edu (media-lab.media.mit.edu) by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA15138; Mon, 27 Jul 1992 16:53:59 +0200 
Received: by media-lab.mit.edu (5.57/DA1.0.3)
	id AA10927; Mon, 27 Jul 92 10:53:56 -0400
Message-Id: <9207271453.AA10927@media-lab.mit.edu>
To: linus@lysator.liu.se
Cc: asb@media.mit.edu (Amy Bruckman), asb@media-lab.mit.edu
Subject: Re: One more cool pizza party/MUD discussion! 
In-Reply-To: Your message of "Mon, 27 Jul 92 03:06:50 +0200."
             <199207270106.AA19820@robin.lysator.liu.se> 
Date: Mon, 27 Jul 92 10:53:56 -0400
From: asb@media-lab.mit.edu
X-Mts: smtp

*** EOOH ***
To: linus@lysator.liu.se
Cc: asb@media.mit.edu (Amy Bruckman), asb@media-lab.mit.edu
Subject: Re: One more cool pizza party/MUD discussion! 
In-Reply-To: Your message of "Mon, 27 Jul 92 03:06:50 +0200."
             <199207270106.AA19820@robin.lysator.liu.se> 
Date: Mon, 27 Jul 92 10:53:56 -0400
From: asb@media-lab.mit.edu
X-Mts: smtp



What a shame our parties overlap!  Why just a minute ago, I got
a message from someone saying he wishes he could come, but
unfortunately he's flying to Sweden for *your* party!  ;-)

This will be our third party.  It's been a lot of fun.  This is
part of a research project on social phenomena in MUDs.

Hope yours is fun!

Cheers,

Amy
Mara@TrekMUSE
jaime@lots-o-places


1, forwarded, answered,,
Summary-line:  7-Aug         rms@gnu.ai.mit.edu  14 #Finally an end to forgotten `;' last in c and c++ header files.
Received: from mole.gnu.ai.mit.edu by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA12812; Sat, 8 Aug 1992 03:25:11 +0200 
Received: by mole.gnu.ai.mit.edu (5.65/4.0)
	id <AA04010@mole.gnu.ai.mit.edu>; Fri, 7 Aug 92 21:25:08 -0400
Date: Fri, 7 Aug 92 21:25:08 -0400
From: rms@gnu.ai.mit.edu (Richard Stallman)
Message-Id: <9208080125.AA04010@mole.gnu.ai.mit.edu>
To: linus@lysator.liu.se
In-Reply-To: Linus Tolke Y's message of 6 Aug 92 07:58:26 <LINUS.92Aug6075826@robin.lysator.liu.se>
Subject: Finally an end to forgotten `;' last in c and c++ header files.

*** EOOH ***
Date: Fri, 7 Aug 92 21:25:08 -0400
From: rms@gnu.ai.mit.edu (Richard Stallman)
To: linus@lysator.liu.se
In-Reply-To: Linus Tolke Y's message of 6 Aug 92 07:58:26 <LINUS.92Aug6075826@robin.lysator.liu.se>
Subject: Finally an end to forgotten `;' last in c and c++ header files.

This might be good to add to Emacs, but I can't understand it.  That's
because you didn't write any commets about how the code is intended to
do its job.  You describbed what it does, bbut not how.  There are
magic constants such as 59 and I don't know what those are or why they
are there.  (This should bbe a character constant, not an integer!)

Can you add suitablke comments and send me the new version?


1,,
Summary-line: 24-Nov  to: slb@nutmeg.ntu.edu.au  27 #Hi from Linkoeping Sweden
Received: from robin.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA21408; Tue, 24 Nov 1992 02:14:45 +0100 
          (rfc931-sender: linus@robin.lysator.liu.se)
Received: by robin.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA03838; Tue, 24 Nov 1992 02:14:36 +0100
          (rfc931-sender: linus@robin.lysator.liu.se)
Date: Tue, 24 Nov 1992 02:14:36 +0100
From: linus@lysator.liu.se
Message-Id: <199211240114.AA03838@robin.lysator.liu.se>
To: Shani Bryceson <slb@nutmeg.ntu.edu.au>
Subject: Hi from Linkoeping Sweden
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Tue, 24 Nov 1992 02:14:36 +0100
From: linus@lysator.liu.se
To: Shani Bryceson <slb@nutmeg.ntu.edu.au>
Subject: Hi from Linkoeping Sweden

Jag tr{ffade din "syster" ]sa Ekberg idag och ber{ttade att jag hade
pratat med dig. Hon tyckte det var roligt och sade att jag skulle
skicka en h{lsning tillbaka.

Det visade sig att det var den ]sa Ekberg som jag k{nner till. Hon har
b|rjat studera Teknisk Fysik och Elektroteknik (Y) ocks} i }r, allts}
samma utbildningslinje som jag (och du).  Hon tycker det {r kul med
matematiken.

Vi ses v{l i SvenskMUD n}gon g}ng!

Adressen till SvenskMUD {r:
svmud.lysator.liu.se 2043
lysator.liu.se 2043
130.236.254.1 2043
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, forwarded, answered,,
Summary-line:  2-Dec       redin@lysator.liu.se  47 #[holm@cs.umu.se (]ke Holmlund): Re: November m}nads SYSTAT ifr}n
Received: from robert.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA27500; Wed, 2 Dec 1992 21:01:23 +0100 
          (rfc931-sender: redin@robert.lysator.liu.se)
Received: by robert.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA11076; Wed, 2 Dec 1992 21:01:18 +0100
          (rfc931-sender: redin@robert.lysator.liu.se)
Date: Wed, 2 Dec 92 21:01:13 MET
From: Magnus Redin <redin@lysator.liu.se>
To: lyskom@lysator.liu.se
Subject: [holm@cs.umu.se (]ke Holmlund): Re: November m}nads SYSTAT ifr}n
        Lysator]
Message-Id: <CMM.0.90.0.723326473.redin@robert.lysator.liu.se>
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Wed, 2 Dec 92 21:01:13 MET
From: Magnus Redin <redin@lysator.liu.se>
To: lyskom@lysator.liu.se
Subject: [holm@cs.umu.se (]ke Holmlund): Re: November m}nads SYSTAT ifr}n
        Lysator]

Received: from jupiter.cs.umu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA25275; Mon, 30 Nov 1992 11:10:15 +0100 
Received: by jupiter.cs.umu.se (5.61-bind 1.5+ida/91-02-01) id AA16399; Mon, 30 Nov 92 11:10:13 +0100
Return-Path: <holm@cs.umu.se>
Date:  Mon, 30 Nov 92 11:10:13 +0100
From: holm@cs.umu.se (]ke Holmlund)
Message-Id: <9211301010.AA16399@jupiter.cs.umu.se>
To: redin@lysator.liu.se
Cc: holm@cs.umu.se
Subject: Re: November m}nads SYSTAT ifr}n Lysator
Newsgroups: swnet.general,nordunet.nucc,liu.lysator,alt.clubs.compsci
References: <2027@lysator.liu.se>
X-Charset: LATIN1
X-Char-Esc: 29

In swnet.general you write:


>	LysKOM	Just nu k|r servern version 1.2.7
>		Senaste version av elispklienten {r: 0.34.6
>		TTYklienten {r fortfarande pre-alfa MEN nu
>		hackar ett par lysiter p} den.
>		Det finns {ven en nyutvecklad Pearl klient.
>		Om du vill testk|ra s} logga in som lyskom p}
>		lysator.liu.se

Jag har n}gra funderingar runt Pearl-klienten. Hur utvecklad {r den 
j{mf|rt med TTY-klienten? G}r den att f} tag i? [r det troligt att den
kommer att utvecklas snabbare eller l}ngsammare {n TTY-klienten?

Anledningen till fr}gorna {r att vi p} ADB i Ume} har behov av ett
konferenssystem men elisp-klienten {r inte m|jlig att anv{nda av 
olika orsaker.

-------------------------------------------------------------------------------
]ke Holmlund				Tel: 090 - 16 57 16
Ume} Universitet			Fax: 090 - 16 61 26
Informationsbehandling			Email: holm@cs.umu.se
901 87 Ume}



1,, bounced,
Summary-line:  7-Dec     to: joshua@Veritas.COM  66 #optimistic compiling
Received: from robin.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA29313; Mon, 7 Dec 1992 04:36:43 +0100 
          (rfc931-sender: linus@robin.lysator.liu.se)
Received: by robin.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA00388; Mon, 7 Dec 1992 04:36:37 +0100
          (rfc931-sender: linus@robin.lysator.liu.se)
Date: Mon, 7 Dec 1992 04:36:37 +0100
From: linus@lysator.liu.se
Message-Id: <199212070336.AA00388@robin.lysator.liu.se>
To: joshua@Veritas.COM (Joshua Levy)
Cc: ceder@lysator.liu.se
In-Reply-To: joshua@Veritas.COM's message of 18 Nov 92 23:02:04 GMT <1992Nov18.230204.1329@Veritas.COM>
Subject: optimistic compiling
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 7 Dec 1992 04:36:37 +0100
From: linus@lysator.liu.se
To: joshua@Veritas.COM (Joshua Levy)
Cc: ceder@lysator.liu.se
In-Reply-To: joshua@Veritas.COM's message of 18 Nov 92 23:02:04 GMT <1992Nov18.230204.1329@Veritas.COM>
Subject: optimistic compiling

You wrote:
   I want to try implmenting "optimistic compiling" with emacs.

   Optimistic compiling is simple: everytime an editor saves a .c 
   file, it forks a job to compile it in the background.  If a 
   previous job is compiling the same file, it is killed first.

   The idea is this: if you modify several .c files, all but the 
   last one will be .o files by the time you hit the main make
   command.  This assumes you are working on a machine which can
   compile and edit without slowing down the edits much.

   Any ideas how to do this?  I know the raw materials are in there
   somewhere, because the compile command does some of what I want.
   I just want it to automatically fire when I save a file, and I 
   need to change the command line slightly based on the file being
   saved.

A few months back I had an other idea that led me to something that
could give you this.

What led to this was the fact that whenever I started a compilation
(M-x compile) it took several seconds for the make to parse the
makefile and start the first compilation.

The idea was that the emacs would parse the makefile and start all the
compilations. Then, if I want the emacs could start news compilations
whenever a file is saved. (The idea included a emacs-global max number
of processes compiling). Whenever a compilation is finished, the
buffer is parsed for errors (why do this while waiting when you can do
it asynchronously? :-) and then if there are news compilations to be
done they are started.

This is essentially a make within emacs and I called it make.el. 

Benefits:
- The verification that a file is up to date took (only) approximately
  twice as much time as the ordinary gnu-make including the parsing of
  the makefile and approximately half the time all following
  compilations, that is when the emacs already have the Makefile
  read-in.

- The problems with it is that the parsing of the error messages and
  the way I use to calculate what is the next file to be compiled are
  rather slow so the emacs just seems to stop working with this. I
  usually want to use my emacs for the editing and not only for
  calculating how to start the next process.
  This could perhaps be solved with some limitations, your idea that
  we are satisfied when we get the .o files might help a bit.

If you want to have a look at my (half finished) code please tell me
and I will send you a copy.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 10-Dec         to: holm@cs.umu.se  42 #Linus }sikter om utvecklingen av olika LysKOM-klienter
Received: from tistel.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA13864; Thu, 10 Dec 1992 00:21:39 +0100 
          (rfc931-sender: linus@tistel.lysator.liu.se)
Received: by tistel.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA05826; Thu, 10 Dec 1992 00:21:33 +0100
          (rfc931-sender: linus@tistel.lysator.liu.se)
Date: Thu, 10 Dec 1992 00:21:33 +0100
From: linus@lysator.liu.se
Message-Id: <199212092321.AA05826@tistel.lysator.liu.se>
To: holm@cs.umu.se (]ke Holmlund)
In-Reply-To: holm@cs.umu.se's message of Mon, 30 Nov 92 11:10:13 +0100 <9211301010.AA16399@jupiter.cs.umu.se>
References: <2027@lysator.liu.se> <9211301010.AA16399@jupiter.cs.umu.se>
Cc: lyskom@lysator.liu.se, dov@menora.weizmann.ac.il, matpe@lysator.liu.se
Cc: redin@lysator.liu.se
Subject: Linus }sikter om utvecklingen av olika LysKOM-klienter
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Thu, 10 Dec 1992 00:21:33 +0100
From: linus@lysator.liu.se
To: holm@cs.umu.se (]ke Holmlund)
In-Reply-To: holm@cs.umu.se's message of Mon, 30 Nov 92 11:10:13 +0100 <9211301010.AA16399@jupiter.cs.umu.se>
References: <2027@lysator.liu.se> <9211301010.AA16399@jupiter.cs.umu.se>
Cc: lyskom@lysator.liu.se, dov@menora.weizmann.ac.il, matpe@lysator.liu.se
Cc: redin@lysator.liu.se
Subject: Linus }sikter om utvecklingen av olika LysKOM-klienter

I ditt brev till Redin skriver du:
    Jag har n}gra funderingar runt Pearl-klienten. Hur utvecklad {r den 
    j{mf|rt med TTY-klienten? G}r den att f} tag i? [r det troligt att den
    kommer att utvecklas snabbare eller l}ngsammare {n TTY-klienten?

    Anledningen till fr}gorna {r att vi p} ADB i Ume} har behov av ett
    konferenssystem men elisp-klienten {r inte m|jlig att anv{nda av 
    olika orsaker.

Det {r ingen som vet svaret p} dessa fr}gor. Allt utvecklingsarbete i
LysKOM har skett och kommer att ske p} ideel basis, dvs att om n}gon
vill g|ra det s} g|r han det och om ingen vill s} blir det h{ngande.

Utvecklingen av den s.k. perl-klienten g|rs av Dov Grobgeld
<dov@menora.weizmann.ac.il> i Israel.

Utvecklingen ttyklienten har i princip st}tt still sedan version 0.02
(som jag "sl{ppte" efter att ha fixat en eller tv} buggar i en av
Bellmans <bellman@lysator.liu.se> allra f|rsta versioner). Nu g}r den
nog inte l{ngre att kompilera f|r vi har {ndrat i vissa bibliotek som
beh|vs.

Perl-klienten var nog t{nkte mest som en bas f|r att kunna skriva en
X-klient mha wafe (jfr xwafenews) och det var helt Dov Grobgeld's ide.
Som den ser ut nu {r den inte att t{nka p} f|r vanliga anv{ndare men
om ni normalt anv{nder X kan det nog bli intressant.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 11-Dec     to: news@solace.hsh.se  33 #Klagom}l p} newstrafiken.
Received: from ruben.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA09013; Fri, 11 Dec 1992 21:48:11 +0100 
          (rfc931-sender: linus@ruben.lysator.liu.se)
Received: by ruben.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA18450; Fri, 11 Dec 1992 21:47:48 +0100
          (rfc931-sender: linus@ruben.lysator.liu.se)
Date: Fri, 11 Dec 1992 21:47:48 +0100
From: linus@lysator.liu.se
Message-Id: <199212112047.AA18450@ruben.lysator.liu.se>
To: news@solace.hsh.se
Subject: Klagom}l p} newstrafiken.
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Fri, 11 Dec 1992 21:47:48 +0100
From: linus@lysator.liu.se
To: news@solace.hsh.se
Subject: Klagom}l p} newstrafiken.

Jag h|rde att ni f}tt klagom}l p} att news-trafiken tog upp mycket
bandbredd. Vi har kanske varit lite f|r frikostiga med vilka grupper
vi skickar och hur mycket det egentligen blivit. Jag har nu minskat p}
volymen h{rifr}n till att bara omfatta det intressantaste (efter vad
jag f|rst}tt fr}n dina brev): liu,luth och stacken. Du f}r v{l allt
annat n}gonannanstans ifr}n hoppas jag. Du f}r nog ocks} minsta lite
p} allt som du frikostigt skickar till oss om det skall m{rkas. Vi har
under tiden fram tills nu f}tt v{ldigt mycket fr}n er, n{stan h{lften
har kommit d{rifr}n.

Minska lite p} antalet grupper, och ocks} p} antalet batchar. Sl{pp
inga batchar mellan 15 och 19 (isy h}ller news till oss under dagen
och b|rjar skicka klockan 17). Som det {r nu har du skickat hela
eftermiddagen och sedan har isy f|rs|kt skicka igen strax efter.

En annan sak. Vi h}ller p} att flytta news-hanteringen till
quetzalcoatl.lysator.liu.se, f|r att avlasta v}r server. Se till att
du verkligen skickar till news.lysator.liu.se, s} skall det inte vara
n}got problem {r det t{nkt.

news.lysator.liu.se/lysator.liu.se:liu,luth,stacken/all:...
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 15-Dec  zappo@titan.ucc.umass.edu 135 #etalk 0.6B
Received: from titan.ucc.umass.edu by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA00444; Tue, 15 Dec 1992 19:19:57 +0100 
Received: by titan.ucc.umass.edu (5.57/Ultrix3.0-C)
	id AA25298; Tue, 15 Dec 92 13:18:45 -0500
Date: Tue, 15 Dec 92 13:18:45 -0500
From: zappo@titan.ucc.umass.edu (Eric Ludlam v 2.1a)
Message-Id: <9212151818.AA25298@titan.ucc.umass.edu>
To: linus@lysator.liu.se
Subject: etalk 0.6B
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Tue, 15 Dec 92 13:18:45 -0500
From: zappo@titan.ucc.umass.edu (Eric Ludlam v 2.1a)
To: linus@lysator.liu.se
Subject: etalk 0.6B

Greetings,

   It has been quite a while since I've written you all about my emacs
talk program.  I'd like to release 0.6B for you all to enjoy.  It has
been running well here for most of the semester.  Changes since the
last version are quite major, and the diffs are a bit bigger than the
source itself.  All the improvments are listed below, and many of you
who wrote complaints to me may recognize a fix or two here or there. ;)
  Hopefully this version will appear more friendly to the rest of the
emacs environment than the last one.  It uses nice names for people
and all sorts of other kool things. 
  Some information of note for everyone is:
  1) You must be running ntalk for this to work (still)
  2) hpux still won't compile the c source.
  3) some games mentioned in the text here will be placed in a 
     different file on the same machine listed below in a week or so.
  This will probably be my last beta release before trying to
understand the Ohio state archiving system.  On a personal level, I am
graduating from UMASS this winter, and loosing my accounts here.
Therefore, I would like to place v. 1 on the net in January with any
stuggestions / help you offer.  After that, my work on this project
may be spotty at best based on long distance dial charges and the
like.

How to get this way kool program!
ftp anonymously to nic.umass.edu [128.119.166.14]
                in /pub/contrib/etalk0.6B.tar.Z
look for my gamebin of additional games in the file:
                   /pub./contrib/egames0.6B.tar.Z

The game bin will be uploaded after my last final exam in a week.

Please let me know any problems/fixes/ideas you may have.
Thank you all for helping.
Eric

-------------------------------------------------
Upgrade changes since 0.4 in order of appearance.
-------------------------------------------------
documentation:
o A TEXINFO file!  I'm very happy about this, although it could use a
  little work.

startup:
o make file adds some improved stuff for .emacs file plus install target.
o version variable for talk and finger modes. Weee
o talk-inhibit-startup-message to prevent blurb thing.
o Optional command line start.  Set alias "etalk" so the unix
  commandline "etalk person" talks to them.

minibuffer:
o Backup finger command in case the 1st one doesn't work
o New improved finger header parser for completion.  Makes additions
  much easier.
o GNU finger supported correctly. ;)
o VMS finger supported.
o More error messages to see.
o Smart completion of tty based on idle time.
o Process 4 days idle? Ask if you really want to talk them.
o New minibuffer temporary messages that look spiffy
o Completion that acts more like completion with SPC and TAB
o New completion key M-TAB will look a persons stats up.
o New host parser only goes when needed 1st time.  
o Large host table? Ask if you want to parse it.

binary:
o more goofy mmessages!
o elisp variable to control goofy message timing.
o a couple improved error messages.

status line:
o Talk version is in it!
o Better way kool configuration variables for buffer names.
o Pull up finger information for both sides of a connection.

buffers:
o Optional buffer clear on reconnection.
o Improved process names to allow more connections to a single account.
o auto-fill-mode implemented correctly as toggle
o fill-mode improved with "soft" and "hard" columns to protect slower
  talk processes from extraneous deletion of text.
o Improved window allocator
o Toss one more window of optional buffer on top of screen.
o New connection propigator.  If you have one link, and the person you
  are talking to is talking to you and 1 or more others, you may
  request connections to everyone else via person 1 and connect
  invisiby to them without going through normal talk channels.

editing:
o emacs->emacs delete past beginning of line to previous line
o include file into conversation
o include file into a seperate buffer for both sides of a conversation

games and tyrants:
o more robust tyrant mode support for games
o drop4 (like connect4)
o 4 by 4
o word-thing (like scrabble)
o checkers
o x dots!
o x reversi
o x chinese checkers
o x order and chaos
o x ataxx
o x battleship (not mine)
o x chess (not mine)
o x gofish (not mine)
  x means that the games are in a different file.
o minibuffer prompt won't flip out anymore.
o optionally ask play game query in talk buffer instead of minibuffer
  to protect sigio misalligned machines.
o better win/loose messages
o usage of real names in messages instead of "player1"
o vt100 style arrow keys can be reassigned to emacs directional keys
  for games for ease of play.
o tyrant-ai to play games agains ai opponant (drop 4 only)

hanging up:
o C-c C-c from local buffer optionally hangs up on everyone for
  multi-person connections.
o C-c C-c from any talk buffer when there are no active processes will
  delete all buffers tagged as being a "talk buffer" including games.
o Process sentinel will redraw the windows on exit, unless you aren't
  in a talk buffer.

other:
o used some new lispy things i learned
o optimized code space-wise in many points
o optimized talk filter.  it runs _much_ faster now.


1,,
Summary-line: 19-Dec  o: kjell-e@lysator.liu.se  37 #sista (?) TGIF
Received: from ruben.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA27241; Sat, 19 Dec 1992 04:33:50 +0100 
          (rfc931-sender: linus@ruben.lysator.liu.se)
Received: by ruben.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA04725; Sat, 19 Dec 1992 04:33:39 +0100
          (rfc931-sender: linus@ruben.lysator.liu.se)
Date: Sat, 19 Dec 1992 04:33:39 +0100
From: linus@lysator.liu.se
Message-Id: <199212190333.AA04725@ruben.lysator.liu.se>
To: kjell-e@lysator.liu.se
Cc: d91matbj@und.ida.liu.se
Subject: sista (?) TGIF
Subject: Sista TGIF f|r s{songen
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Sat, 19 Dec 1992 04:33:39 +0100
From: linus@lysator.liu.se
To: kjell-e@lysator.liu.se
Cc: d91matbj@und.ida.liu.se
Subject: sista (?) TGIF
Subject: Sista TGIF f|r s{songen

Oj, ett s}nt tempo.
L}tarna:
	Mats Ronander	Suzy solidor		VPP
	Vibe		Ain't no sunshine	NY
	Vibe		Feel Free		PB
Intervju med Fredrik fr}n Vibe
	Peps Blodsband	F|r livet		PB
	Svenne Rubns	Gubbe, e du dum i huvve	PB (typisk kjell-e l}t.)
	Indecent Obsess	Indio			PB
	Cecilia Ray	Love Gives no guarantee	NY
Pratar med Cecilia via telefon
	Cecilia Ray	Make it right		NY
Pratar mer (detta {r Mattias Bj|rklunds andra intervju n}gonsin. Den
f|rsta var med Vibe (se ovan)).
	Cecilia Ray	Move On			PB beatmixat med...
	Martika		Martikas Kitchen	
	Pernilla W	C'est Demon		PB
	Vanessa P	I godda have it	
	Tribe Wanbessa	Never ever		PB
	Lars Vegas Tria	Hej tomtegubbar		(jag hade 25 sekunder |ver).

Den korta jingeln: Thank God its Friday k|rde jag kopi|st mycket.
Minst en g}ng i varje l}t, i en del l}tar upp till 10 g}nger. :-)
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 19-Dec  Rimley-Evans@cl.CAm.ac.UK  27 #Emacs suggestion
Received: from varg.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA02178; Sat, 19 Dec 1992 07:00:55 +0100 
          (rfc931-sender: linus@varg.lysator.liu.se)
Received: by varg.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA00572; Sat, 19 Dec 1992 07:00:50 +0100
          (rfc931-sender: linus@varg.lysator.liu.se)
Date: Sat, 19 Dec 1992 07:00:50 +0100
From: linus@lysator.liu.se
Message-Id: <199212190600.AA00572@varg.lysator.liu.se>
To: Edmund.GRimley-Evans@cl.CAm.ac.UK
In-Reply-To: Edmund.GRimley-Evans@cl.CAm.ac.UK's message of Thu, 17 Dec 1992 16:38:53 GMT
Subject: Emacs suggestion
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Sat, 19 Dec 1992 07:00:50 +0100
From: linus@lysator.liu.se
To: Edmund.GRimley-Evans@cl.CAm.ac.UK
In-Reply-To: Edmund.GRimley-Evans@cl.CAm.ac.UK's message of Thu, 17 Dec 1992 16:38:53 GMT
Subject: Emacs suggestion

   Newsgroups: gnu.emacs.bug
   From: Edmund.GRimley-Evans@cl.CAm.ac.UK
   Organization: GNUs Not Usenet
   Distribution: gnu
   Date: Thu, 17 Dec 1992 16:38:53 GMT

   It might be useful to make the definition of "word" parameterisable, as
   is the definition of "paragraph". This would make it easier to define
   modes for other languages in which the set of characters allowable in a
   "word" may vary.

Isn't the syntax table construction good enough for you? I know there
are problems with it, especially when defining a multiletter comment
syntax but for this it works.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 24-Dec        liberte@cs.uiuc.edu  11 #edebug testing
Received: from a.cs.uiuc.edu by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA08504; Thu, 24 Dec 1992 18:36:07 +0100 
Received: by a.cs.uiuc.edu id AA09647
  (5.64+/IDA-1.3.4 for ); Thu, 24 Dec 92 10:40:55 -0600
Received: from ebony.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA09643
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Thu, 24 Dec 92 10:40:52 -0600
Message-Id: <9212241640.AA09643@a.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA00879; Thu, 24 Dec 92 10:40:49 -0600
Date: Thu, 24 Dec 92 10:40:49 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug testing
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Thu, 24 Dec 92 10:40:49 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug testing

I will soon send out the next version of edebug.
Since this mailing list has seen little activity, I thought I should
test it first.

dan


1,,
Summary-line: 24-Dec  atek!hollen@a.cs.uiuc.edu  24 #edebug testing
Received: from a.cs.uiuc.edu by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA23594; Fri, 25 Dec 1992 02:33:38 +0100 
Received: by a.cs.uiuc.edu id AA14005
  (5.64+/IDA-1.3.4 for ); Thu, 24 Dec 92 18:33:07 -0600
Received: from harvard.UUCP by a.cs.uiuc.edu with UUCP id AA13786
  (5.64+/IDA-1.3.4 for ); Thu, 24 Dec 92 17:53:58 -0600
Received: by harvard.harvard.edu (5.54/a0.25)
	(for edebug) id AA07530; Thu, 24 Dec 92 18:53:27 EST
Received: from ra.megatek 
	by megatek.UUCP (4.1/smail2.5/09-29-87)
	id AA18117; Thu, 24 Dec 92 15:17:19 PST
Received: from peg.megatek by ra.megatek (4.1/SMI-4.1)
	id AA27565; Thu, 24 Dec 92 15:17:16 PST
Date: Thu, 24 Dec 92 15:17:16 PST
Message-Id: <9212242317.AA27565@ra.megatek>
From: harvard!scubed!megatek!hollen@a.cs.uiuc.edu (Dion Hollenbeck)
To: liberte@cs.uiuc.edu
Cc: edebug@cs.uiuc.edu
In-Reply-To: uunet!cs.uiuc.edu!liberte's message of Thu, 24 Dec 92 10:40:49 -0600 <9212241640.AA09643@a.cs.uiuc.edu>
Subject: edebug testing
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Thu, 24 Dec 92 15:17:16 PST
From: harvard!scubed!megatek!hollen@a.cs.uiuc.edu (Dion Hollenbeck)
To: liberte@cs.uiuc.edu
Cc: edebug@cs.uiuc.edu
In-Reply-To: uunet!cs.uiuc.edu!liberte's message of Thu, 24 Dec 92 10:40:49 -0600 <9212241640.AA09643@a.cs.uiuc.edu>
Subject: edebug testing

>>>>> On Thu, 24 Dec 92 10:40:49 -0600, uunet!cs.uiuc.edu!liberte said:

dan> I will soon send out the next version of edebug.
dan> Since this mailing list has seen little activity, I thought I should
dan> test it first.

dan> dan

I am alive and well and still interested in edebug.  Thank you once
again for a WONDERFUL tool and Happy Holidays to you.

dion

Dion Hollenbeck                        Email: hollen@megatek.com
Senior Software Engineer                      megatek!hollen@uunet.uu.net
Megatek Corporation, San Diego, California    ucsd!megatek.uucp!hollen


1,,
Summary-line: 25-Dec              MAILER-DAEMON  56 #Returned mail: Service unavailable
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA01202; Fri, 25 Dec 1992 06:28:19 +0100 
Date: Fri, 25 Dec 1992 06:28:19 +0100
From: MAILER-DAEMON (Mail Delivery Subsystem)
Subject: Returned mail: Service unavailable
Message-Id: <199212250528.AA01202@lysator.liu.se>
To: owner-pdp11
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Fri, 25 Dec 1992 06:28:19 +0100
From: MAILER-DAEMON (Mail Delivery Subsystem)
Subject: Returned mail: Service unavailable
To: owner-pdp11

   ----- Transcript of session follows -----
connection to 130.236.254.133: Connection timed out
giving up...
Couldn't connect to quetzalcoatl.lysator.liu.se news server, try again later.
451 Cannot exec /usr/lyskom/bin/getmail: Connection timed out during delivery with /usr/lyskom/bin/getmail
554 1141@lyskom.lysator.liu.se... Service unavailable

   ----- Unsent message follows -----
Received: from transarc.com by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA01200; Fri, 25 Dec 1992 06:28:19 +0100 
Received: by transarc.com (5.54/3.15) id <AA03621>; Fri, 25 Dec 92 00:02:42 EST
Received: via switchmail for INFO-PDP11_Mailing-List@transarc.com;
 Fri, 25 Dec 1992 00:02:40 -0500 (EST)
Received: from transarc.com via qmail
          ID </afs/transarc.com/service/mailqs/q1/QF.sfCdJET0Bi818Bv05a>;
          Fri, 25 Dec 1992 00:00:01 -0500 (EST)
Received: from plains.NoDak.edu (plains.NoDak.edu, [134.129.111.64]) by transarc.com (5.54/3.15) id <AA03559> for info-pdp11; Thu, 24 Dec 92 23:59:50 EST
Received: by plains.NoDak.edu; Thu, 24 Dec 1992 22:59:45 -0600
Date: Thu, 24 Dec 1992 22:59:45 -0600
From: Todd Enders - WD0BCI <enders@plains.nodak.edu>
Message-Id: <199212250459.AA18145@plains.NoDak.edu>
To: info-pdp11@transarc.com
Subject: Q-bus MFM controllers?


Happy Holidays all!

     As a Christmas present, I received an old 20 Mb MFM hard drive (Computer
Memories Inc. 6426-S) that's supposed to work.  Is there a relatively
inexpensive q-bus controller available that'd handle such a beast.  I assume
one couldn't hook one up to an RQDX-2(3).  Anything from Emulex or Dilog
that one might likely find surplus (i.e. more or less cheap)?  Emulation
wouldn't be critical, as long as it were useable by RSX, but MSCP or RL-01/02
would be nice as I could boot from it if need be.  Many thanks for any and all
leads!

Todd

===============================================================================

Todd Enders - WD0BCI                  ARPA: enders@plains.nodak.edu
Computer Center                       UUCP: ...!uunet!plains!enders
Minot State University                  or: ...!hplabs!hp-lsd!plains!enders
Minot, ND  58701                     Bitnet: enders@plains

     "The present would be full of all possible futures,
      if the past had not already projected a pattern upon it" - Andre' Gide

===============================================================================



1,,
Summary-line: 26-Dec  Stensture@svmud.lysator.l  21 #Emailadress
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA11440; Sat, 26 Dec 1992 19:10:01 +0100 
          (rfc931-sender: svmud@lysator.liu.se)
Date: Sat, 26 Dec 1992 19:10:01 +0100
Message-Id: <199212261810.AA11440@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Stensture@svmud.lysator.liu.se (Stensture i Svenskmud)
Reply-To: butterr@solace.krynn.hsh.se (Stensture i Svenskmud)
X-From-Mud-User: Stensture i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Emailadress
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Sat, 26 Dec 1992 19:10:01 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Stensture@svmud.lysator.liu.se (Stensture i Svenskmud)
Reply-To: butterr@solace.krynn.hsh.se (Stensture i Svenskmud)
X-From-Mud-User: Stensture i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Emailadress

Brev från Stensture i SvenskMUD.

God Jul !
Kan du vara v{nlig att kolla upp min emailadress?
Jag vet inte om den st{mmer, men annars vet Radagast nog vad den {r.
Mitt anv{ndarnamn {r Butter, s} han kan kolla om det st{mmer.
Var sn'll att verifiera detta !

Gott nytt }r !

Butter / Thomas Eriksson / Stensture


1,,
Summary-line: 27-Dec              MAILER-DAEMON  46 #Returned mail: Service unavailable
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA06467; Sun, 27 Dec 1992 23:09:39 +0100 
Date: Sun, 27 Dec 1992 23:09:39 +0100
From: MAILER-DAEMON (Mail Delivery Subsystem)
Subject: Returned mail: Service unavailable
Message-Id: <199212272209.AA06467@lysator.liu.se>
To: owner-pdp11
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Sun, 27 Dec 1992 23:09:39 +0100
From: MAILER-DAEMON (Mail Delivery Subsystem)
Subject: Returned mail: Service unavailable
To: owner-pdp11

   ----- Transcript of session follows -----
connection to 130.236.254.133: Connection timed out
giving up...
Couldn't connect to quetzalcoatl.lysator.liu.se news server, try again later.
451 Cannot exec /usr/lyskom/bin/getmail: Connection timed out during delivery with /usr/lyskom/bin/getmail
554 1141@lyskom.lysator.liu.se... Service unavailable

   ----- Unsent message follows -----
Received: from transarc.com by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA06456; Sun, 27 Dec 1992 23:09:39 +0100 
Received: by transarc.com (5.54/3.15) id <AA01878>; Sun, 27 Dec 92 16:41:09 EST
Received: via switchmail for INFO-PDP11_Mailing-List@transarc.com;
 Sun, 27 Dec 1992 16:41:08 -0500 (EST)
Received: from transarc.com via qmail
          ID </afs/transarc.com/service/mailqs/q1/QF.ofDW9uz0Bi8187J047>;
          Sun, 27 Dec 1992 16:39:07 -0500 (EST)
Received: from depot.cis.ksu.edu (depot.cis.ksu.edu, [129.130.10.5]) by transarc.com (5.54/3.15) id <AA01871> for info-pdp11; Sun, 27 Dec 92 16:39:00 EST
Return-Path:  <uucp@mccall.uucp>
Received: from mccall.UUCP by depot.cis.ksu.edu UUCP (5.65a) 
		id AA13174; Sun, 27 Dec 92 15:38:57 -0600
Received: by mccall.com (DECUS UUCP /1.3-1/2.5/);
          Sun, 27 Dec 92 15:27:58 CST
To: mccall!info-pdp11-newsgate
Subject: FORSALE: SDI Cables for RA60,RA80 & RA81
Message-Id: <BzxqDv.H5r@ccu.umanitoba.ca>
From: rflukes@ccu.umanitoba.ca (Richard F. Lukes)
Date: Sun, 27 Dec 1992 20:32:18 GMT
Sender: news@ccu.umanitoba.ca
Organization: University of Manitoba, Winnipeg, Canada

I have two BC26V-12 cables plus a mounting panel which I would like
to sell.  Best offer takes them.  Willing to sell separately.

Thanks
--Rich
-- 
Richard F. Lukes                    rflukes@ccu.UManitoba.CA
Computer Science Department         (or rflukes@ccm.UManitoba.CA)
University of Manitoba
WORK: (204)-474-8696                HOME: (204)-257-6701


1,,
Summary-line: 28-Dec              MAILER-DAEMON  57 #Returned mail: Service unavailable
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA12570; Mon, 28 Dec 1992 02:17:07 +0100 
Date: Mon, 28 Dec 1992 02:17:07 +0100
From: MAILER-DAEMON (Mail Delivery Subsystem)
Subject: Returned mail: Service unavailable
Message-Id: <199212280117.AA12570@lysator.liu.se>
To: owner-pdp11
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 28 Dec 1992 02:17:07 +0100
From: MAILER-DAEMON (Mail Delivery Subsystem)
Subject: Returned mail: Service unavailable
To: owner-pdp11

   ----- Transcript of session follows -----
connection to 130.236.254.133: Connection timed out
giving up...
Couldn't connect to quetzalcoatl.lysator.liu.se news server, try again later.
451 Cannot exec /usr/lyskom/bin/getmail: Connection timed out during delivery with /usr/lyskom/bin/getmail
554 1141@lyskom.lysator.liu.se... Service unavailable

   ----- Unsent message follows -----
Received: from transarc.com by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA12568; Mon, 28 Dec 1992 02:17:07 +0100 
Received: by transarc.com (5.54/3.15) id <AA02425>; Sun, 27 Dec 92 19:46:41 EST
Received: via switchmail for INFO-PDP11_Mailing-List@transarc.com;
 Sun, 27 Dec 1992 19:46:39 -0500 (EST)
Received: from transarc.com via qmail
          ID </afs/transarc.com/service/mailqs/q2/QF.4fDYrfn0Bi8189Qk4o>;
          Sun, 27 Dec 1992 19:44:28 -0500 (EST)
Received: from depot.cis.ksu.edu (depot.cis.ksu.edu, [129.130.10.5]) by transarc.com (5.54/3.15) id <AA02414> for info-pdp11; Sun, 27 Dec 92 19:44:18 EST
Return-Path:  <uucp@mccall.uucp>
Received: from mccall.UUCP by depot.cis.ksu.edu UUCP (5.65a) 
		id AA20340; Sun, 27 Dec 92 18:44:16 -0600
Received: by mccall.com (DECUS UUCP /1.3-1/2.5/);
          Sun, 27 Dec 92 18:28:12 CST
To: mccall!info-pdp11-newsgate
Subject: FORSALE: Qbus PDP11/03 & /23 systems
Message-Id: <Bzxr7B.HAt@ccu.umanitoba.ca>
From: rflukes@ccu.umanitoba.ca (Richard F. Lukes)
Date: 27 Dec 92 20:49:59 GMT
Sender: news@ccu.umanitoba.ca
Organization: University of Manitoba, Winnipeg, Canada

I have three DEC BA11-M Qbus enclosures which I would like to sell or
give away.  Two where used as 11/03 systems,  while the third was used
as a Qbus expansion chassis.  The BA11-M has 4 quad 18-bit Qbus slots.
If someone would like to have the enclosure,  plus 11/03 CPU, 64KB,
bootstrap terminator, console I/O, plus hardware manuals and schematics,
then I'll let them go for $75US each.  A 22-bit 11/23 CPU board would
be an additional $100US.  Shipping is not included.

If someone just wants the empty BA11-M enclosure,  I am willing to
let that go for *FREE* if UPS shipping is prepaid.  I also have a spare
11/03 power supply which I would like to give away.

I have lots of stuff,  so please ask if you are interested.

Thanks,
--Rich
--
Richard F. Lukes                    rflukes@ccu.UManitoba.CA
Computer Science Department         (or rflukes@ccm.UManitoba.CA)
University of Manitoba
WORK: (204)-474-8696                HOME: (204)-257-6701


1,,
Summary-line: 28-Dec                    kjell-e  10 #Re:  sista (?) TGIF Sista TGIF f|r s{songen
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA27529; Mon, 28 Dec 1992 09:33:06 +0100 
          (rfc931-sender: kjell-e@lysator.liu.se)
Date: Mon, 28 Dec 1992 09:33:06 +0100
From: kjell-e (Kjell Enblom)
Message-Id: <199212280833.AA27529@lysator.liu.se>
To: linus@lysator.liu.se
Subject: Re:  sista (?) TGIF Sista TGIF f|r s{songen
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 28 Dec 1992 09:33:06 +0100
From: kjell-e (Kjell Enblom)
To: linus@lysator.liu.se
Subject: Re:  sista (?) TGIF Sista TGIF f|r s{songen

Vadd} typisk kjell-e-l}t....hmf...  :-)
L}ter som ett bra program, fast kanske en aningens tjatande med en viss jingel... Synd att jag inte kunde lyssna. Bra musikval.
 
                        Gott nytt }r p} dig /kjell-e


1,,
Summary-line: 28-Dec  Fred@svmud.lysator.liu.se  17 #strul
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA14421; Mon, 28 Dec 1992 18:10:00 +0100 
          (rfc931-sender: svmud@lysator.liu.se)
Date: Mon, 28 Dec 1992 18:10:00 +0100
Message-Id: <199212281710.AA14421@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Fred@svmud.lysator.liu.se (Fred i Svenskmud)
Reply-To: t92frelo@ida.liu.und (Fred i Svenskmud)
X-From-Mud-User: Fred i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: strul
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 28 Dec 1992 18:10:00 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Fred@svmud.lysator.liu.se (Fred i Svenskmud)
Reply-To: t92frelo@ida.liu.und (Fred i Svenskmud)
X-From-Mud-User: Fred i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: strul

Brev från Fred i SvenskMUD.

tjba.
 
jag är en liten ny magiker (20e dec). när jag fick mitt slott (av ST) så gick lysators
maskin ner så jag fick nivå 21 men inget slott... Testola har nu hjälp mig med att få ett
slott. bara så du vet... 


1, answered,,
Summary-line:  3-Jan         pbn@lysator.liu.se  37 #News
Received: from varg.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA06094; Sun, 3 Jan 1993 00:17:53 +0100 
          (rfc931-sender: pbn@varg.lysator.liu.se)
Received: by varg.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA08570; Sun, 3 Jan 1993 00:17:44 +0100
          (rfc931-sender: pbn@varg.lysator.liu.se)
Date: Sun, 3 Jan 1993 00:17:44 +0100
From: pbn@lysator.liu.se
Message-Id: <199301022317.AA08570@varg.lysator.liu.se>
To: linus@lysator.liu.se
Subject: News
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Sun, 3 Jan 1993 00:17:44 +0100
From: pbn@lysator.liu.se
To: linus@lysator.liu.se
Subject: News

Hej Linus,

Hoppas julen och det nya }ret var/{r bra!

Jag har motionerat quetzalcoatl lite nu efter ny}r och d} fr{mst
m.h.a.  News. Jag har sett att det d} och d}, typ ett par g}nger per
dygn, startar n}got som skapar l}sfilen ~news/LOCK och som sedan
terminerar utan att ta bort l}set efter sej. Jag har en k{nsla av att
detta har bidragit till att inte news har packat upp sej under julen
p} det autonoma s{tt man skulle kunna ha hoppats p}.

N}v{l nu undrar jag om du, som f}r s}dana, har mail-loggar som kan
visa vad som d|r utan att st{da efter sej, senas var det en process
med PID 20537 som gjorde s} h{r och denna process hade sin d|ds}ngest
mellan 23:30 - 0:10 den 2:a januari 93.

Det verkar inte som att News riktigt klarar att l|sa denna l}sning s}
som den {r just nu.

Hmm... Kan det vara relaynews som inte st{dar efter sej? Den skapar ju
LOCK.  N}ja du kanske vet om n}gon mer skapar den? Jag har nog tyckt
att relaynews verkar st{da omkring sej men den kanske f}r spatt n}gon
g}ng?

Det blir ju problem med att packa upp allt inom en s} h{r kort tide
eftersom s} g}tt som allt startar sin expire-tid samtidigt... Detta
g|r ju att inte News blir sej riktigt likt f|rr{n vi f}tt allt
uppackat och *d{refter* har rullat news s} att alla grupper f}tt skifta
ut expire ordentligt n}got varv...

	/pbn - 21 timmar och n{rmare sextiotusen artiklar senare... :)


1, answered,,
Summary-line:  3-Jan  Nordgrimm@svmud.lysator.l  26 #Fusk
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA13567; Sun, 3 Jan 1993 04:10:02 +0100 
          (rfc931-sender: svmud@lysator.liu.se)
Date: Sun, 3 Jan 1993 04:10:02 +0100
Message-Id: <199301030310.AA13567@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Nordgrimm@svmud.lysator.liu.se (Nordgrimm i Svenskmud)
Reply-To: gandalf@lysator.liu.se (Nordgrimm i Svenskmud)
X-From-Mud-User: Nordgrimm i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Fusk
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Sun, 3 Jan 1993 04:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Nordgrimm@svmud.lysator.liu.se (Nordgrimm i Svenskmud)
Reply-To: gandalf@lysator.liu.se (Nordgrimm i Svenskmud)
X-From-Mud-User: Nordgrimm i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Fusk

Brev från Nordgrimm i SvenskMUD.

Dent (21) gav Faxanadu/Stensture 2000 daler var under helgen.
Jag fixade det s} att Stensture blev av med en 750 daler plus
ett i int 15-14 och Faxanadu blev bara av med int 16-15.
Jag tycker det r{cker som straff f|r dom verkar vara okej
typer egentligen. Elric {r min testgubbe ifall du undrar.
Annars h}ller jag p} och bygger p} mitt gille mer.
Testa klona gille/amulett ifall du {r nyfiken.
                   God Jul och Gott nytt ]r i efterskott/ Vincent


1,,
Summary-line:  4-Jan   milamber@krynn.solace.hs  18 #ange-ftp.elc
Received: from rudolf.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA10248; Mon, 4 Jan 1993 11:16:28 +0100 
          (rfc931-sender: linus@rudolf.lysator.liu.se)
Received: by rudolf.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA29456; Mon, 4 Jan 1993 11:16:50 +0100
          (rfc931-sender: linus@rudolf.lysator.liu.se)
Date: Mon, 4 Jan 1993 11:16:50 +0100
From: linus@lysator.liu.se
Message-Id: <199301041016.AA29456@rudolf.lysator.liu.se>
To: milamber@krynn.solace.hsh.se (Per Persson)
In-Reply-To: milamber@krynn.solace.hsh.se's message of Fri, 1 Jan 1993 05:23:23 MET
Subject: ange-ftp.elc
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 4 Jan 1993 11:16:50 +0100
From: linus@lysator.liu.se
To: milamber@krynn.solace.hsh.se (Per Persson)
In-Reply-To: milamber@krynn.solace.hsh.se's message of Fri, 1 Jan 1993 05:23:23 MET
Subject: ange-ftp.elc

Ange filnamnet: /milamber@themud.mashine 4801:minfil...
					^ ett mellanslag.
Problemet {r att f} in ett mellanslag utan att den g|r en massa
skumheter men d} anv{nder man givetvis C-q SPC som ju stoppar in utan
att klaga.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  4-Jan  to: %LysKOM.lysator.liu.s  53 #Dina postningar:
Received: from rudolf.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA10671; Mon, 4 Jan 1993 11:28:35 +0100 
          (rfc931-sender: linus@rudolf.lysator.liu.se)
Received: by rudolf.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA29471; Mon, 4 Jan 1993 11:29:00 +0100
          (rfc931-sender: linus@rudolf.lysator.liu.se)
Date: Mon, 4 Jan 1993 11:29:00 +0100
From: linus@lysator.liu.se
Message-Id: <199301041029.AA29471@rudolf.lysator.liu.se>
To: "Person 118"%LysKOM.lysator.liu.se@lysator.liu.se   (Redin)
Cc: redin@lysator.liu.se
In-Reply-To: "Redin"@LysKOM.lysator.liu.se's message of Tue, 22 Dec 1992 22:00:16
Subject: Dina postningar:
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 4 Jan 1993 11:29:00 +0100
From: linus@lysator.liu.se
To: "Person 118"%LysKOM.lysator.liu.se@lysator.liu.se   (Redin)
Cc: redin@lysator.liu.se
In-Reply-To: "Redin"@LysKOM.lysator.liu.se's message of Tue, 22 Dec 1992 22:00:16
Subject: Dina postningar:

Problemet var antagligen att allting l}ste sig (n}got program har g}tt
snett som inte skulle det) och sedan har allting bara legat och v{ntat
p} att stoppas in. Ig}r b|rjade pbn fixa med det hela s} sedan dess
har den nog stoppats in.

Jag har kollat och bara 2 av dina postningar har packats upp och lagts
in:
Newsgroups: swnet.politik
From: redin@lysator.liu.se (Magnus Redin)
Subject: Re: reklam
Message-ID: <BzoB3p.FCG@lysator.liu.se>

ahrvid@sfbbs.edvina.se (A Engholm) writes:
>  1) Reklamen skapar ett overhead p} priset, utan att konsumenten tillfr}gats
>om man ovanp} priset vill betala reklamen. Kostnaden f|r reklam l{ggs ju
>p} priset. Vill man k|pa produkter utan att betala reklam-overhead s} f}r
>man det sv}rt. Det finns f} produkter som saknar reklam-overhead.
Det h{r {r ett informationsproblem. Konsumenten har f|r sv}rt
att hitta bra varor att k|pa. En l|sning kan vara att se till
att konsumenten kan finna skalig infromatin om produkter p}
et enkelt s{tt.
...

Newsgroups: swnet.politik
From: redin@lysator.liu.se (Magnus Redin)
Subject: Re: Maastricht-avtalet
Message-ID: <BzoGwF.Hv8@lysator.liu.se>

d1dd@dtek.chalmers.se (Daniel Deimert) skriver:
>Nej, det {r inte s} l{sbart. Och det packar grymt bra, s} det inneh}ller
>mycket |verfl|dig information. (L}nga ord, m}nga upprepningar, mycket
>politiska slogans utan inneb|rd.)
Intresant m}tt p} inneh}ll...

N}gon som har tips om b|cker som beskriver statsmodeller som inte "packar"
s} bra och inneh}ller bara de ves{ntliga funktionerna?
N}got som f|ljer principen: "V{rlden {r komplicerad och den g}r inte
att f|renkla med komplicerade regler"
...
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  4-Jan  to: rsr@plague-ether.berk  24 #removing comments...
Received: from robert.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA14741; Mon, 4 Jan 1993 13:34:34 +0100 
          (rfc931-sender: linus@robert.lysator.liu.se)
Received: by robert.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA17923; Mon, 4 Jan 1993 13:34:30 +0100
          (rfc931-sender: linus@robert.lysator.liu.se)
Date: Mon, 4 Jan 1993 13:34:30 +0100
From: linus@lysator.liu.se
Message-Id: <199301041234.AA17923@robert.lysator.liu.se>
To: rsr@plague-ether.berkeley.edu
Subject: removing comments...
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 4 Jan 1993 13:34:30 +0100
From: linus@lysator.liu.se
To: rsr@plague-ether.berkeley.edu
Subject: removing comments...

(defun tabortcomment ()
  "Removes comments from c-code."
  (interactive)
  (save-excursion
    (while (re-search-forward "/\\*\\([^*]\\|\\*[^/]\\|[\n\r]\\)*\\*/" nil t)
      (if (or (nth 3 (parse-partial-sexp (point-min) (match-beginning 0)))
	      (nth 3 (parse-partial-sexp (point-min) (match-end 0))))
	  (progn
	    (goto-char (match-beginning 0))
	    (while (nth 3 (parse-partial-sexp (point-min) (point)))
	      (forward-char 1)))
	(replace-match "")))))
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, forwarded,,
Summary-line:  7-Jan           marsj@ida.liu.se  93 #Medley - Processer i medley
Received: from ida.liu.se (curofix.ida.liu.se) by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA17334; Thu, 7 Jan 1993 14:00:32 +0100 
Received: from obelix by ida.liu.se (5.65b/ida.minimaster-V1.0b6d5)
	id AA25782; Thu, 7 Jan 93 14:00:28 +0100
From: Martin Sjolin <marsj@ida.liu.se>
Received: from obel27 by obelix (5.65b/ida.slave-V1.0b3)
	id AA22783; Thu, 7 Jan 93 14:00:27 +0100
Received: by obel27 (5.65b/ida.slave-V1.0b3)
	id AA22197; Thu, 7 Jan 93 14:00:26 +0100
Date: Thu, 7 Jan 93 14:00:26 +0100
Message-Id: <9301071300.AA22197@obel27>
To: "Linus Tolke Y." <linus@lysator.liu.se>:
Subject: Medley - Processer i medley
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
From: Martin Sjolin <marsj@ida.liu.se>
Date: Thu, 7 Jan 93 14:00:26 +0100
To: "Linus Tolke Y." <linus@lysator.liu.se>:
Subject: Medley - Processer i medley

Linus,

h_r f_ljer en nedtecknad (ur manualen) om processer:

Processer & Medley
------------------

(IL:ADD.PROCESS form) som skapar en process, notera att INGA
v_rden komemr att _verf_ras fr_n den aktuella omgivning. Eventuella
v_rden f_r anges som parametrar till form. Tex givet att funktionen
foo beh_ver det g_llande v_rdet av fie och fum, kan vi
 
  (IL:ADD.PROCESS `(FOO ,FIE ,FOO))

vilket start en process anropet till funktion foo!

(IL:THIS.PROCESS) returnerar aktuell process id (g_ende).

(IL:DEL.PROCESS proc) "d_dar" process proc, d_r proc kan var 
v_rdet returnerat fr_n IL:ADD.PROCESS eller namnet! (Kan ses
mha Control-G).

(IL:PROCESS.RETURN value) avsluate den aktuella processen och 
returnerar v_rdet value. J_mf_r med BLOCK -- RETURN i Common Lisp.

(IL:PROCESSP proc) sant om proc anger en process.

(IL:PROCESS.FINISHEDP proc) sant om proc _r avslutad.

(IL:MAP.PROCESS mapfn) mapfunktion _ver alla processer. mapfn tar
tre argument, process (handle/id), namn och form (se add.process).

(IL:BLOCK msecswait timer) - l_mar _ver kontrollen till n_sta process
i k_n, msecwait anger antal sekunder innan vi kommer tillbaka. T anger
att vi v_nter f_revigt. Kan dock brytas med IL:WAKE.PROCESS

(IL:WAKE.PROCESS proc status) -- v_cker funktione proc och BLOCK/SUSPEND
etc returnerar status.

(IL:SUSPEND.PROC proc) -- suspend processen proc till den f_r en WAKE.PROCESS

(IL:PROCESS.EVAL proc form waitforresult) -- evaluerar form i stack-
omgivning f_r processen proc. Om waitforresult _r sant, v_nte p_ att
resultat blir tillg_ngligt, annars k_rs de tv_ processerna parallelt.
Notera att eventuella fel kommer att erh_llas i proc!!!

(IL:PROCESS.APPLY proc fn args waitforresult) -- se f_reg_ende funktion.

(IL:CREATE.EVENT name) -- skapar en event med namnet name (se nedan)

(IL:AWAIT.EVENT event timeout) -- suspend aktuell process tills den
erh_ller en event. Om timeout nil kommer ingen timeout att anv_ndas,
annars anges antalet mS.

(IL:NOTIFY.EVENT event onceonly) -- Om det finns processer som
v_ntar p_ (await) event, v_ck dem! Om onceonly _r sant, v_ck 
endast den f_rsta processen.


(IL:CREATE.MONITORLOCK name) -- skapar en instance av monitorlock
data typen, som skall anv_ndas i lock funktionerna nedan. Namn _r
godtyckligt och kan anv_ndas f_r fels_kning/status.

(IL:WITH.MONITORLOCK lock form1 ... form2) -- evaluerar form1 t.o.m.
formn medans vi "_ger" lock och returnerar v_rdet av fromn. Obs _r
dynamiskt scopat - e.g. om den aktuella processen redan _ger lock,
s_ v_ntar inte anropet.

(IL:MONITOR.AWAIT.EVENT releaselock event timeout timerp) --
Utf_r en (IL:AWAIT.EVENT event timeout timerp), men sl_pper
releaselock f_rst, och f_rs_ker sedan _terf_ releaselock (kan
d_ sova).

(IL:OBTAIN.MONITORLOCK lock dontwait unwindsave) -- tar _ver lock,
v_ntar om n_dv_ndigt om dontwait ej _r t d_ returneras nil direkt. 
Returnerar lock om det kunde erh_llas, t ifall processen redan
hade lock annars nil (kunde inte f_ lock direkt).

(IL:RELEASE.MONITORLOCK lock evenifnotmine) -- sl_pper lock och
v_cker n_sta process som v_ntar p_ lock. Om evenifnotmin _r t
s_ sl_pps lock _ven om process inte _ger den.

---------------
Martin Sj\"olin                                   e-mail: marsj@ida.liu.se 
Intelligent Information Systems Laboratory        phone : +46 13 28 24 10
Department of Computer and Information Science    fax   : +46 13 28 26 66
University of Link\"oping, S-581 83 Link\"oping, SWEDEN


1,,
Summary-line:  8-Jan        paul@lysator.liu.se  98 #mailinglista, vill ha
Received: from robin.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA13074; Fri, 8 Jan 1993 01:48:01 +0100 
          (rfc931-sender: paul@robin.lysator.liu.se)
Received: by robin.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA12846; Fri, 8 Jan 1993 01:47:38 +0100
          (unknown)
Date: Fri, 8 Jan 1993 01:47:38 +0100
From: paul@lysator.liu.se
Message-Id: <199301080047.AA12846@robin.lysator.liu.se>
Subject: mailinglista, vill ha
Apparently-To: linus@lysator.liu.se
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Fri, 8 Jan 1993 01:47:38 +0100
From: paul@lysator.liu.se
Subject: mailinglista, vill ha
Apparently-To: linus@lysator.liu.se

Path: lysator.liu.se!isy!liuida!sunic!sunet-gateway!owner
From: gvaudre@CNRI.Reston.VA.US (Greg Vaudreuil)
Newsgroups: nordunet.redist.ietf
Subject: WG-ACTION: Simple IP (sip)
Message-ID: <9301071551.aa07673@IETF.CNRI.Reston.VA.US>
Date: 7 Jan 93 20:51:26 GMT
Sender: List Manager <Postmaster@CNRI.Reston.VA.US>


A new working group has formed in the Internet area.  For more information
please contact the working group chairs or the area directors.


Simple Internet Protocol (sip)

Charter 
 
Chair(s):
     Christian Huitema  <christian.huitema@sophia.inria.fr>
     Steve Deering      <deering@parc.xerox.com>
 
Internet Area Director(s) 
     Philip Almquist  <almquist@jessica.stanford.edu>
     Stev Knowles     <stev@ftp.com>
     Dave Piscitello  <dave@sabre.bellcore.com>
 
Mailing lists: 
     General Discussion:sip@caldera.usc.edu
     To Subscribe:      sip-request@caldera.usc.edu
     Archive:           

Description of Working Group:

   SIP is another candidate for IPv7. The purpose of the Working Group
   is to finalize the SIP family of protocol, and to foster the early
   development and experimentation of this protocol.

   There are two major characteristics of the SIP proposal: it is very
   much a continuation of IP, and it aims at maximum simplicity. A
   short hand definition of SIP could be ``64 bits IP with useless
   overhead removed''.

   Following the IP model, SIP uses globally-unique addresses,
   hierarchically structured for efficient routing. SIP addresses are
   64 bits long, which we believe adequate to scale the Internet up
   to,  say, thousands of internet-addressable devices in every office,
   every residence, and every vehicle in the world.

   The quest of simplicity in SIP has been described as parallel to the
   RISC philosophy. The minimal SIP header contain only those fields
   which are necessary to achieve our goal: routing packets efficently
   in a very large internet. As a result of this design philosophy, the
   SIP header is much simpler than the IP header. Simplicity
   facilitates high-performance implementation and increases the
   likelihood of correct implementation.

   Contrarily to several other IPv7 candidates, the SIP effort is
   focused mostly on the description of the final state, not on the
   description of the transition. This is due to a coordination with
   the IPAE working group, which has already engaged an intensive study
   of transition problems, with SIP in mind as a final state.

Goals and Milestones: 
 
     Done Post the complete SIP specification as an Internet-Draft. This 
          specification shall include the header format, the address format, 
          ICMP and IGMP, the fragmentation protocol, the source route protocol,
          and the the requirements SIP imposes on higher layer protocols and 
          lower later protocols, e.g., ARP.                                    

   Jan 93 Post an Internet-Draft specifing the SIP addressing and routing 
          architecture. Include discussion of multicast and mobile host support
          as well as a discussion of how policy routing can be supported. 
          Detail the changes required to OSPF, BGP, and RIP.                   

   Jan 93 Post as an Internet-Draft a specification for the SIP MIB. Detail the
          operation of SNMP over SIP.                                          

   Jan 93 Make available a public domain implementation of SIP for the UNIX-BSD
          socket environment.                                                  

   Jan 93 Make available a public domain version of modified TCP and UDP for 
          the UNIX-BSD socket environment.                                     

   Mar 93 Post as an Internet-Draft a report on the initial implementation and 
          experience with SIP.                                                 

   Jun 93 Incorporate security into SIP.                                       

   Jun 93 Specify in detail the changes to the routing protocols needed for 
          SIP.                                                                 



1,,
Summary-line:  8-Jan       ceder@lysator.liu.se  33 #release -d deletes too little - and too much
Received: from konrad.lysator.liu.se by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA13116; Fri, 8 Jan 1993 01:49:06 +0100 
          (rfc931-sender: ceder@konrad.lysator.liu.se)
Received: by konrad.lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA02077; Fri, 8 Jan 1993 01:49:02 +0100
          (rfc931-sender: ceder@konrad.lysator.liu.se)
Date: Fri, 8 Jan 1993 01:49:02 +0100
From: ceder@lysator.liu.se
Message-Id: <199301080049.AA02077@konrad.lysator.liu.se>
To: info-cvs@prep.ai.mit.edu
Subject: release -d deletes too little - and too much
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Fri, 8 Jan 1993 01:49:02 +0100
From: ceder@lysator.liu.se
To: info-cvs@prep.ai.mit.edu
Subject: release -d deletes too little - and too much

Too little:
-----------

Perhaps "cvs release -d foo" should run "rm -rf" instead of "rm -r" if
the module is checked out with "cvs -r checkout" (making the files
read-only)? It is boring to have to reply "y" for every file there is
in the module...

I think there is no reason not to use "-rf" instead of "-r" all the
time.

Too much:
---------

1. Check out a module.
2. mkdir a directory within the module.
3. Move all your favourite files to the new directory.
4. "cvs release -d module".
5. Weep, as you realise that step 4 above silently deleted the foreign
   directory and all the files within it without any warning. An "U
   file" message is printed for every unknown file in CVS-controlled
   directories, but not for other files.

The release command should print a warning when it encounters an
"unknown" directory (a directory without a CVS subdirectory).

				/ceder


1,,
Summary-line: 18-Jan        liberte@cs.uiuc.edu  11 #edebug version 2.8 now available
Received: from a.cs.uiuc.edu by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA04987; Mon, 18 Jan 1993 21:39:32 +0100 
Received: by a.cs.uiuc.edu id AA13395
  (5.64+/IDA-1.3.4 for ); Mon, 18 Jan 93 13:43:51 -0600
Received: from ebony.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA13391
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Mon, 18 Jan 93 13:43:48 -0600
Message-Id: <9301181943.AA13391@a.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA05809; Mon, 18 Jan 93 13:43:45 -0600
Date: Mon, 18 Jan 93 13:43:45 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug version 2.8 now available
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 18 Jan 93 13:43:45 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug version 2.8 now available

It's been almost a year since my last release.  Sounds like a cross between
confession and ... never mind.  This time its available via anonymous ftp
on a.cs.uiuc.edu in /tmp/edebug.tar.Z.  Let me know if there are problems.

dan


1,,
Summary-line: 18-Jan  Gunnar@svmud.lysator.liu.  16 #Ekstaven
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA08302; Mon, 18 Jan 1993 23:10:01 +0100 
          (rfc931-sender: svmud@lysator.liu.se)
Date: Mon, 18 Jan 1993 23:10:01 +0100
Message-Id: <199301182210.AA08302@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@svmud.lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Ekstaven
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 18 Jan 1993 23:10:01 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@svmud.lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Ekstaven

Brev från Gunnar i SvenskMUD.

Jag har hittat en liten brist i ekstaven. Kommandot testa uppt{cker inte
om rustning {r av n}gon besynnerlig typ som till exempel 0. Jag lokaliserade
felet till funktionen test_armour_object och t{nkte r{tta det. Men det
fick jag inte s} vars}god.


1,,
Summary-line: 18-Jan  Dent@svmud.lysator.liu.se  26 #Restaurangen Milliways
Received: by lysator.liu.se 
          (5.65c8/1.34/Lysator-3.1) id AA08307; Mon, 18 Jan 1993 23:10:03 +0100 
          (rfc931-sender: svmud@lysator.liu.se)
Date: Mon, 18 Jan 1993 23:10:03 +0100
Message-Id: <199301182210.AA08307@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@svmud.lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Restaurangen Milliways
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 18 Jan 1993 23:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@svmud.lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Restaurangen Milliways

Brev från Dent i SvenskMUD.


Haj hopp !

Nu har jag helt fr{ckt '|ppnat' mitt omr}de utan att
h|ra med n}gon... S} nu g|r jag det.
[r en s}n h{r sak alldeles f|r 'off the wall' eller ?
Mitt omr}de b|rjar vid /spelare/radagast/kullarna/ku5, g}
ner och lyssna till den underbara musiken !


tacksam f|r svar,

        Dent



1,,
Summary-line: 18-Jan        liberte@cs.uiuc.edu  24 #Re:  More Edebug bugs
Received: from a.cs.uiuc.edu by lysator.liu.se  with SMTP
          (5.65c8/1.34/Lysator-3.1) id AA09422; Mon, 18 Jan 1993 23:41:06 +0100 
Received: by a.cs.uiuc.edu id AA15402
  (5.64+/IDA-1.3.4 for ); Mon, 18 Jan 93 16:14:56 -0600
Received: from ebony.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA15398
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Mon, 18 Jan 93 16:14:50 -0600
Message-Id: <9301182214.AA15398@a.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA05850; Mon, 18 Jan 93 16:14:48 -0600
Date: Mon, 18 Jan 93 16:14:48 -0600
From: liberte@cs.uiuc.edu
To: mernst@theory.lcs.mit.edu
Subject: Re:  More Edebug bugs
Cc: edebug@cs.uiuc.edu
X-Charset: LATIN1
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 18 Jan 93 16:14:48 -0600
From: liberte@cs.uiuc.edu
To: mernst@theory.lcs.mit.edu
Subject: Re:  More Edebug bugs
Cc: edebug@cs.uiuc.edu

There must have been a floppy disk error.  I'll have to make a new copy.
dan

----------

	The LCD Archive Entry of custom-print.el has an empty date and revision.
	What's worse, the file ends after 16384 characters:

			 (setcdr tag id)
			 (setq id (1- id))
			 (setq rest (cdr rest)))
			;; Else delete this object.
			(t (setcdr rest (cdr (cdr r

						-Mike




1,,
Summary-line: 19-Jan      Pontus@lysator.liu.se  21 #query_namnet()
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA10544; Tue, 19 Jan 1993 00:10:01 +0100
Date: Tue, 19 Jan 1993 00:10:01 +0100
Message-Id: <199301182310.AA10544@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Pontus@lysator.liu.se (Pontus i Svenskmud)
Reply-To: adb91pbe@hfb-aes.hfb.se (Pontus i Svenskmud)
X-From-Mud-User: Pontus i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: query_namnet()

*** EOOH ***
Date: Tue, 19 Jan 1993 00:10:01 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Pontus@lysator.liu.se (Pontus i Svenskmud)
Reply-To: adb91pbe@hfb-aes.hfb.se (Pontus i Svenskmud)
X-From-Mud-User: Pontus i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: query_namnet()

Brev från Pontus i SvenskMUD.

Hej Linus!
Funktionen query_namnet() returnerar en spelares namn, oavsett om
han/hon/den {r osynlig eller ej. Det {r litet tr}kigt om man vill
vara osynlig och ett monster eller dylikt avsl|jar ens namn.  Det
{r nog ganska m}ngar magiker som anv{nder den funktionen.   Borde
inte funktionen returnera 'N}gon' n{r en spelare {r synlig??  Jag bara u
Jag bara undrade...

	Pontus


1,,
Summary-line: 19-Jan        Dent@lysator.liu.se  21 #igen !
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA10549; Tue, 19 Jan 1993 00:10:04 +0100
Date: Tue, 19 Jan 1993 00:10:04 +0100
Message-Id: <199301182310.AA10549@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: igen !

*** EOOH ***
Date: Tue, 19 Jan 1993 00:10:04 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: igen !

Brev från Dent i SvenskMUD.


Haj igen !

Jag l{ste just igenom REGLERna f|r denna mud...
Attan. Det kanske inte {r n}gon bra ide... Jag har
iallafall st{llt en butig vakt iv{gen f|r ing}ngen.
M}nga brev blir det...

 / Dent


1,,
Summary-line: 19-Jan     Gwentar@lysator.liu.se  23 #hopslagning.
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA05283; Tue, 19 Jan 1993 12:10:03 +0100
Date: Tue, 19 Jan 1993 12:10:03 +0100
Message-Id: <199301191110.AA05283@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gwentar@lysator.liu.se (Gwentar i Svenskmud)
Reply-To: tdi9025@abacus.hgs.se (Gwentar i Svenskmud)
X-From-Mud-User: Gwentar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: hopslagning.

*** EOOH ***
Date: Tue, 19 Jan 1993 12:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gwentar@lysator.liu.se (Gwentar i Svenskmud)
Reply-To: tdi9025@abacus.hgs.se (Gwentar i Svenskmud)
X-From-Mud-User: Gwentar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: hopslagning.

Brev från Gwentar i SvenskMUD.

Jag tittade lite p} min v{rld och omliggande v{rldar och uppt{ckte
att jag enkelt skulle kunna sl} ihop min v{rld med Masens v{rld
i norr (Jag har ju redan en f|rbindelse med Testola i s|der).
Men jag tyckte att det var b{st att fr}b dej f|rst.
Masens v{rld passar precis ihop med min om jag l{gger till ett rum.
Jag har skrivit ett f|rslag /spelare/gwentar/trollskog/trollskog86
som jag inte |ppnat {n, den skulle f|rbindas med masen/rum/Stig6 i
|ster och masen/rum/Skog1 i norr. Jag har inte pratat med masen {n
utan t{nkte h|ra om det {r okej med dej f|rst.
Mvh Gwentar den sovande.
alias tdi9025@abacus.hgs.se


1,,
Summary-line: 19-Jan  hollen%megatek%scubed%har  22 #More Edebug bugs
Received: from nanny.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA04121; Wed, 20 Jan 1993 11:13:33 +0100
Received: from elrond.ida.liu.se by nanny.lysator.liu.se (5.64/1.34) with SMTP
          (5.64/1.34/Lysator-3.0) id AA12137; Tue, 19 Jan 93 20:56:31 GMT 
Received: from a.cs.uiuc.edu by elrond.ida.liu.se with SMTP
	(5.61-bind 1.5X+ida/IDA-1.2.8-mc2.5-2) id AA26178; Tue, 19 Jan 93 21:53:21 +0100
Received: by a.cs.uiuc.edu id AA02424
  (5.64+/IDA-1.3.4 for ); Tue, 19 Jan 93 13:39:14 -0600
Received: from harvard.UUCP by a.cs.uiuc.edu with UUCP id AA01434
  (5.64+/IDA-1.3.4 for ); Tue, 19 Jan 93 12:42:02 -0600
Received: by harvard.harvard.edu (5.54/a0.25)
	(for edebug) id AA02306; Tue, 19 Jan 93 13:41:27 EST
Received: from ra.megatek 
	by megatek.UUCP (4.1/smail2.5/09-29-87)
	id AA00844; Tue, 19 Jan 93 10:11:19 PST
Received: from peg.megatek by ra.megatek (4.1/SMI-4.1)
	id AA20686; Tue, 19 Jan 93 10:11:17 PST
Date: Tue, 19 Jan 93 10:11:17 PST
Message-Id: <9301191811.AA20686@ra.megatek>
From: hollen%megatek%scubed%harvard@a.cs.uiuc.edu (Dion Hollenbeck)
To: liberte@cs.uiuc.edu
Cc: mernst@theory.lcs.mit.edu, edebug@cs.uiuc.edu
In-Reply-To: uunet!cs.uiuc.edu!liberte's message of Mon, 18 Jan 93 16:14:48 -0600 <9301182214.AA15398@a.cs.uiuc.edu>
Subject:  More Edebug bugs

*** EOOH ***
Date: Tue, 19 Jan 93 10:11:17 PST
From: hollen%megatek%scubed%harvard@a.cs.uiuc.edu (Dion Hollenbeck)
To: liberte@cs.uiuc.edu
Cc: mernst@theory.lcs.mit.edu, edebug@cs.uiuc.edu
In-Reply-To: uunet!cs.uiuc.edu!liberte's message of Mon, 18 Jan 93 16:14:48 -0600 <9301182214.AA15398@a.cs.uiuc.edu>
Subject:  More Edebug bugs

>>>>> On Mon, 18 Jan 93 16:14:48 -0600, uunet!cs.uiuc.edu!liberte said:

Dan> There must have been a floppy disk error.  I'll have to make a new copy.
Dan> dan

Could you please inform when the new copy is available.  FTP from here
is flaky at best and I want to take my best shot at it only when what
is there is known to be good and worth taking.  

Thanks for keeping me informed of the new version.

dion




1,,
Summary-line: 19-Jan        liberte@cs.uiuc.edu  21 #edebug 2.8 problems
Received: from nanny.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA04295; Wed, 20 Jan 1993 11:16:08 +0100
Received: from elrond.ida.liu.se by nanny.lysator.liu.se (5.64/1.34) with SMTP
          (5.64/1.34/Lysator-3.0) id AA10917; Tue, 19 Jan 93 18:54:26 GMT 
Received: from a.cs.uiuc.edu by elrond.ida.liu.se with SMTP
	(5.61-bind 1.5X+ida/IDA-1.2.8-mc2.5-2) id AA25964; Tue, 19 Jan 93 19:51:19 +0100
Received: by a.cs.uiuc.edu id AA00844
  (5.64+/IDA-1.3.4 for ); Tue, 19 Jan 93 12:09:29 -0600
Received: from ebony.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA00839
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Tue, 19 Jan 93 12:09:26 -0600
Message-Id: <9301191809.AA00839@a.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA06208; Tue, 19 Jan 93 12:09:22 -0600
Date: Tue, 19 Jan 93 12:09:22 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug 2.8 problems

*** EOOH ***
Date: Tue, 19 Jan 93 12:09:22 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug 2.8 problems

Problems with edebug 2.8 that I know about so far.  The last is a killer.
So don't bother with edebug 2.8 unless you want to experiment.

	custom-print.el is not complete, as mentioned before.

	byte compiling edebug requires removing old edebug.elc file(s)
	first since loading of edebug is required.  Actually, you could
	probably remove the (require 'edebug) because it may no longer 
	be needed.

	Serious bug: You cannot edebug-defun any defun because it
	acts like an interactive form is required.  Not sure why this
	is happening or how it got past my recent tests.  

dan


1,,
Summary-line: 20-Jan  to: root%quetzalcoatl.lys  20 #/users
Received: from robin.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA19503; Wed, 20 Jan 1993 18:27:24 +0100
Received: by robin.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA02001; Wed, 20 Jan 93 18:27:20 +0100
          (unknown)
Date: Wed, 20 Jan 93 18:27:20 +0100
From: linus@lysator.liu.se
Message-Id: <9301201727.AA02001@robin.lysator.liu.se>
To: root%quetzalcoatl.lysator.liu.se@lysator.liu.se
Subject: /users

*** EOOH ***
Date: Wed, 20 Jan 93 18:27:20 +0100
From: linus@lysator.liu.se
To: root%quetzalcoatl.lysator.liu.se@lysator.liu.se
Subject: /users

N{r jag skall komma }t /users f}r jag n{stan hela tiden:
NFS read failed for server lysator:/users: RPC: Timed out

Kan man inte trolla lite med NFS s} att man f}r:
NFS server lysator not responding still trying
NFS server lysator OK
ist{llet?
Dvs jag kan k|ra mitt program fast det g}r l}ngsamt.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 20-Jan        liberte@cs.uiuc.edu  73 #edebug 2.8 again
Received: from a.cs.uiuc.edu by lysator.liu.se (ALPHA-6.8/6.1) id AA22306; Wed, 20 Jan 1993 19:39:51 +0100
Received: by a.cs.uiuc.edu id AA22290
  (5.64+/IDA-1.3.4 for ); Wed, 20 Jan 93 11:57:34 -0600
Received: from ebony.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA22286
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Wed, 20 Jan 93 11:57:32 -0600
Message-Id: <9301201757.AA22286@a.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA06687; Wed, 20 Jan 93 11:57:28 -0600
Date: Wed, 20 Jan 93 11:57:28 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug 2.8 again

*** EOOH ***
Date: Wed, 20 Jan 93 11:57:28 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug 2.8 again

Edebug version 2.8 is alive again.  It turns out there was no killer bug.
The problem must have been the old edebug.elc again, though it is difficult
to tell because I cannot repeat it now.  

Anyway, make sure *you* remove any old edebug.elc, and byte-compile
the new edebug.el, and load the new edebug.elc if you have an old
one loaded already, or better yet, restart emacs.

I won't release a new version to fix the couple other small problems
until the rest come in.

Below is the changes from 2.7.

dan
---------------------------------
;;; Support edebugging top-level forms and generalize handling
;;; of defining forms.
;;;
;;; Rename edebug-defun to edebug-eval-top-level-form.
;;; edebug-defun still points to the latter.
;;;
;;; Rename edebug-all-defuns to edebug-all-defs.
;;;
;;; Add edebug-all-forms option and command.
;;;
;;; Add edebug-continue-kbd-macro option.
;;;
;;; Stop defining epoch::version.
;;;
;;; Rename def-edebug-form-spec to def-edebug-form.  Arguments are unevaluated.
;;;
;;; edebug-form-spec supports indirection.  List specs may now
;;; contain body, &define, name, arglist, def-body, def-form, and strings.
;;;
;;; While parsing, commit to alternative after matching a symbol.
;;;
;;; Fix nested &optional handling.
;;;
;;; Improve syntax error reporting.
;;;
;;; Use edebug-form-specs for many Emacs special-forms: defun, defmacro,
;;; interactive, condition-case, cond, as well as lambda forms and
;;; functions that take function arguments.  Define specs for all cl.el
;;; macros.
;;;
;;; Fix printing of window objects so they show the correct buffer.
;;;
;;; Numerous display fixes that are too complex to explain.
;;;
;;; Display frequency counts along with coverage data by inserting comment
;;; lines.
;;;
;;; Add global break condition.
;;;
;;; Add "next" mode to stop only after expression evaluation.
;;; Add top-level-nonstop to stop no more.
;;;
;;; Add time argument to edebug-bounce-point.
;;;
;;; Allow editing of previous breakpoint condition.
;;;
;;; Fix edebug-step-in.
;;;
;;; Clean up the backtrace display better.
;;;
;;; Support Lucid Emacs command events.
;;;


1,,
Summary-line: 20-Jan                       pell  10 #Re:  andra saker att fixa n{r ni f}r tid!
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA26443; Wed, 20 Jan 1993 21:32:10 +0100
Date: Wed, 20 Jan 1993 21:32:10 +0100
From: P{r Emanuelsson <pell>
Message-Id: <199301202032.AA26443@lysator.liu.se>
To: linus@lysator.liu.se
Subject: Re:  andra saker att fixa n{r ni f}r tid!

*** EOOH ***
Date: Wed, 20 Jan 1993 21:32:10 +0100
From: P{r Emanuelsson <pell>
To: linus@lysator.liu.se
Subject: Re:  andra saker att fixa n{r ni f}r tid!

Jag kanske borde ha skrivit "ocks}'. Anledningen {r att bl.a. Bellman
har grejat med installationen och han f}r inget mail om du bara
skriver till root.
   /Pell


1, forwarded,,
Summary-line: 21-Jan   Sylvester@lysator.liu.se  14 #Uppdrag 7
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA29076; Thu, 21 Jan 1993 13:10:07 +0100
Date: Thu, 21 Jan 1993 13:10:07 +0100
Message-Id: <199301211210.AA29076@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Sylvester@lysator.liu.se (Sylvester i Svenskmud)
Reply-To: d2hacker@dtek.chalmers.se (Sylvester i Svenskmud)
X-From-Mud-User: Sylvester i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Uppdrag 7

*** EOOH ***
Date: Thu, 21 Jan 1993 13:10:07 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Sylvester@lysator.liu.se (Sylvester i Svenskmud)
Reply-To: d2hacker@dtek.chalmers.se (Sylvester i Svenskmud)
X-From-Mud-User: Sylvester i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Uppdrag 7

Brev från Sylvester i SvenskMUD.

Min emailadress st{mmer i alla fall!
mvh Sylvester


1,,
Summary-line: 21-Jan        liberte@cs.uiuc.edu  55 #Re:  edebug bug
Received: from a.cs.uiuc.edu by lysator.liu.se (ALPHA-6.8/6.1) id AA13651; Thu, 21 Jan 1993 19:42:43 +0100
Received: by a.cs.uiuc.edu id AA17094
  (5.64+/IDA-1.3.4 for ); Thu, 21 Jan 93 12:09:05 -0600
Received: from ebony.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA17085
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Thu, 21 Jan 93 12:08:55 -0600
Message-Id: <9301211808.AA17085@a.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA07008; Thu, 21 Jan 93 12:08:50 -0600
Date: Thu, 21 Jan 93 12:08:50 -0600
From: liberte@cs.uiuc.edu
To: bosch@loria.fr
Subject: Re:  edebug bug
Cc: edebug@cs.uiuc.edu

*** EOOH ***
Date: Thu, 21 Jan 93 12:08:50 -0600
From: liberte@cs.uiuc.edu
To: bosch@loria.fr
Subject: Re:  edebug bug
Cc: edebug@cs.uiuc.edu

	From: Guido Bosch <Guido.Bosch@loria.fr>
	To: Daniel LaLiberte <liberte@cs.uiuc.edu>
	Subject: edebug bug
	Reply-To: Guido Bosch <bosch@loria.fr>

	Hi Dan,

	I encountered a basic bug while trying out the new edebug version:

	Whenever I edebug a form (defun or other), I get the error 

		Wrong type argument: marker-p, nil
		
	after the first blank I type to step into. I tried with the following
	forms: 

	(defun fac (n)
	  (if (< 0 n)
	      (* n (fac (1- n)))
	    1))

	;(fac 3)	

	(mapcar '(lambda (x) x)
		'(a b c))


	This happens for Lucid Emacs19.3, with both compiled and
	interpreted edebug code. 


	If you can't reproduce the error (which seems highly probable to me),
	what can I do to debug edebug (e.g., with the classical frame
	debugger)? 

		Guido

I strongly suspect that this is a problem only with using edebug in
lemacs.  There were several fixes to edebug made to support
lemacs, and my changes since then may have broken one of those
fixes, or required yet another fix.  But I have no idea where the
problem lies.  I think Jamie could identify it immediately however.

Debugging edebug is usually tricky.  I don't use the standard debugger
except to (setq debug-on-error t) and find out where it failed and
examine a few values.

dan


1,,
Summary-line: 21-Jan    Tomtefar@lysator.liu.se  26 #Dödsfällor
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA16735; Thu, 21 Jan 1993 21:10:02 +0100
Date: Thu, 21 Jan 1993 21:10:02 +0100
Message-Id: <199301212010.AA16735@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Tomtefar@lysator.liu.se (Tomtefar i Svenskmud)
Reply-To: t92tj@hh.se (Tomtefar i Svenskmud)
X-From-Mud-User: Tomtefar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Dödsfällor

*** EOOH ***
Date: Thu, 21 Jan 1993 21:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Tomtefar@lysator.liu.se (Tomtefar i Svenskmud)
Reply-To: t92tj@hh.se (Tomtefar i Svenskmud)
X-From-Mud-User: Tomtefar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Dödsfällor

Brev från Tomtefar i SvenskMUD.

		Hejsan!
	Är det egentligen meningen att det ska finnas dödsfällor i svmud?
Tor i Asgård har blivit en sådan. Jag gick in som nivå 19. Styrka och hälsa 20.
Skyddet låg på 26. Siffrorna hade jag kollat med div magiker här på skolan, Merli bla.
Jag hade även tors hammare som vapen. Jag börjar slåss med honom och kollar poäng under tiden.
När jag var nere på 132 så slår tor 3 gånger på mig, jag en gång på honom, sedan slår han mig 3 gånger till.
Under den tiden görs inga försök att fly trots inkopplat fegishumör.
Efter de 3 sista slagen så kommer det upp en 5-7 OK meddelanden på skärmen och sedan "Du dör".
Allt detta under loppet av ca 3-4 sekunder! Ingen chans att fly alltså. 
Ska det vara så här. Att jag dessutom går från ca 830000 xp till ca 530000, samt att alla egenskaperna
sänktes ett steg, tycker jag är lite i magstarkaste laget.
I dag bad jag Merli kolla upp Tors styrka. Ganska intressant. Jag undrar bara vilken spelare som 
kan slå en figur med bla styrka 45 nivå 20 och skydd 50 samt ett vapen i klass 20 som är magiskt.
Jag tycker att man ska ha en ärlig chans att dra sig ur en strid som är alltför svår. Den här var omöjlig.


1,,
Summary-line: 22-Jan        liberte@cs.uiuc.edu  38 #Re:  edebug.el 2.8 problem
Received: from a.cs.uiuc.edu by lysator.liu.se (ALPHA-6.8/6.1) id AA11437; Fri, 22 Jan 1993 22:57:46 +0100
Received: by a.cs.uiuc.edu id AA12218
  (5.64+/IDA-1.3.4 for ); Fri, 22 Jan 93 14:45:09 -0600
Received: from ebony.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA12177
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Fri, 22 Jan 93 14:42:57 -0600
Message-Id: <9301222042.AA12177@a.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA07434; Fri, 22 Jan 93 14:42:55 -0600
Date: Fri, 22 Jan 93 14:42:55 -0600
From: liberte@cs.uiuc.edu
To: jackr@dblues.wpd.sgi.com
Subject: Re:  edebug.el 2.8 problem
Cc: edebug@cs.uiuc.edu

*** EOOH ***
Date: Fri, 22 Jan 93 14:42:55 -0600
From: liberte@cs.uiuc.edu
To: jackr@dblues.wpd.sgi.com
Subject: Re:  edebug.el 2.8 problem
Cc: edebug@cs.uiuc.edu

The killer bug is back.

	From: Jack Repenning <jackr@dblues.wpd.sgi.com>

	I'm getting that `Invalid read syntax: "expected", "interactive"'
	thing on some defuns, but not on others.  For example, this one, in a
	file all by itself, chokes:


	(defun my-edit-and-eval-command (prompt command)
	  "Prompting with PROMPT, let user edit COMMAND and eval result.
	COMMAND is a Lisp expression.  Let user edit that expression in
	the minibuffer, then read and evaluate the result.
	Personalized: modified or ancient commands are added to the head of
	the command history list."
	  (let ((newcmd (read-minibuffer prompt
	                                 (prin1-to-string command))))
	    (or (equal newcmd (car command-history))
	        (setq command-history (cons newcmd command-history)))
	    (eval newcmd)))



Right you are.  It looks like the "(let" is confusing things because
if you insert a "nil" before it, edebug can parse it.
Or if you take out the doc string, it works.  Weird.

Anybody who needs edebug 2.7 until I fix this, just ask.

dan



1,,
Summary-line: 24-Jan             tpm@ida.liu.se  10 #Re:  nrelayd
Received: from curofix.ida.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA25985; Sun, 24 Jan 1993 11:23:47 +0100
Received: from diagnostix by ida.liu.se (5.65b/ida.minimaster-V1.0b6d5)
	id AA16960; Sun, 24 Jan 93 11:23:44 +0100
From: Thomas Padron-McCarthy <tpm@ida.liu.se>
Received: from diag22 by diagnostix (5.65b/ida.slave-V1.0b3)
	id AA15480; Sun, 24 Jan 93 11:23:43 +0100
Received: by diag22 (5.65b/ida.slave-V1.0b3)
	id AA19741; Sun, 24 Jan 93 11:23:42 +0100
Date: Sun, 24 Jan 93 11:23:42 +0100
Message-Id: <9301241023.AA19741@diag22>
To: linus@lysator.liu.se
Subject: Re:  nrelayd

*** EOOH ***
From: Thomas Padron-McCarthy <tpm@ida.liu.se>
Date: Sun, 24 Jan 93 11:23:42 +0100
To: linus@lysator.liu.se
Subject: Re:  nrelayd

Jo, det verkar ha varit n}n sorts nameserverproblem.
Ska kolla lite mer snart.

-- tpm


1,,
Summary-line: 25-Jan        Kiwi@lysator.liu.se  15 #dörrar.
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA16611; Mon, 25 Jan 1993 14:10:02 +0100
Date: Mon, 25 Jan 1993 14:10:02 +0100
Message-Id: <199301251310.AA16611@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Kiwi@lysator.liu.se (Kiwi i Svenskmud)
Reply-To: t92ma2@hh.se (Kiwi i Svenskmud)
X-From-Mud-User: Kiwi i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: dörrar.

*** EOOH ***
Date: Mon, 25 Jan 1993 14:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Kiwi@lysator.liu.se (Kiwi i Svenskmud)
Reply-To: t92ma2@hh.se (Kiwi i Svenskmud)
X-From-Mud-User: Kiwi i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: dörrar.

Brev från Kiwi i SvenskMUD.

Skulle det inte kunna gå att få riktiga svenska bokstäver i dörr-beskrivningarna? Som det är nu så blir det bara klamrar.
Försökte själv fixa det, men gick inte.....
/KIWI


1,,
Summary-line: 25-Jan        Fred@lysator.liu.se  20 #problem
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA18742; Mon, 25 Jan 1993 15:10:03 +0100
Date: Mon, 25 Jan 1993 15:10:03 +0100
Message-Id: <199301251410.AA18742@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Fred@lysator.liu.se (Fred i Svenskmud)
Reply-To: t92frelo@und.ida.liu.se (Fred i Svenskmud)
X-From-Mud-User: Fred i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: problem

*** EOOH ***
Date: Mon, 25 Jan 1993 15:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Fred@lysator.liu.se (Fred i Svenskmud)
Reply-To: t92frelo@und.ida.liu.se (Fred i Svenskmud)
X-From-Mud-User: Fred i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: problem

Brev från Fred i SvenskMUD.

hejsan

jag har b|rjat med ett nytt litet gille och t{nkte ha med en formel som
ger tillf{lligt |kad styrka. Det {r ju enkelt att |ka styrkan och sedan 
s{tta en flagga och efter en viss tid s{nka styrkan igen.
Problemet {r om n}gon {r dum och slutar medan han eller hon {r stark. D} kommer ju
inte flaggan att vara kvar. Jag kan ju inte legga flaggan i ett autoloadande object heller eftersom de
tydligen inte alltid fungerar som de ska. Har du n}got f|rslag...


1,,
Summary-line: 25-Jan     Gwentar@lysator.liu.se  18 #Nyb|rjarv{rld :-)
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA21136; Mon, 25 Jan 1993 16:10:03 +0100
Date: Mon, 25 Jan 1993 16:10:03 +0100
Message-Id: <199301251510.AA21136@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gwentar@lysator.liu.se (Gwentar i Svenskmud)
Reply-To: tdi9025@abacus.hgs.se (Gwentar i Svenskmud)
X-From-Mud-User: Gwentar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Nyb|rjarv{rld :-)

*** EOOH ***
Date: Mon, 25 Jan 1993 16:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gwentar@lysator.liu.se (Gwentar i Svenskmud)
Reply-To: tdi9025@abacus.hgs.se (Gwentar i Svenskmud)
X-From-Mud-User: Gwentar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Nyb|rjarv{rld :-)

Brev från Gwentar i SvenskMUD.

Hejsan jag och n}gra andra magiker har pratat lita om en nyb|rjarv{rld
p} svenskmuddet och om hur den skall se ut.Det skulle vara meningen
att alla som vill skulle f} bygga p} den och att n}gon("jag?") skulle
sammordna allt sedan.S} nu undrar jag ifall detta skulle vara m|jligt
att genomf|ra :-)
Mvh Gwentar den sovande alias tdi9025@abacus.hgs.se


1,,
Summary-line: 25-Jan   to: ceder@lysator.liu.se  49 #bugtrack-addon f|r oss som f}r buggarna i LysKOM. :-)
Received: from varg.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA00746; Mon, 25 Jan 1993 20:21:08 +0100
Received: by varg.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA00763; Mon, 25 Jan 93 20:21:04 +0100
          (unknown)
Date: Mon, 25 Jan 93 20:21:04 +0100
From: linus@lysator.liu.se
Message-Id: <9301251921.AA00763@varg.lysator.liu.se>
To: ceder@lysator.liu.se
Subject: bugtrack-addon f|r oss som f}r buggarna i LysKOM. :-)

*** EOOH ***
Date: Mon, 25 Jan 93 20:21:04 +0100
From: linus@lysator.liu.se
To: ceder@lysator.liu.se
Subject: bugtrack-addon f|r oss som f}r buggarna i LysKOM. :-)

Jag gjorde ett litet hack f|r elisp-klienten.
Ist{llet f|r att l{sa in Reported by och Site s} stoppar den in namnet
p} den person som skrivet ett inl{gg och LysKOM som site.

Kan du inte fixa en parameter &optional text till bugtrack-create-bug
som att man kan stoppa in kommentarer direkt?

(defun kom-bugtrack (summary severity priority difficulty text-no)
  "Bugtrack from test."
  (interactive "sSummary: 
nSeverity (0=unimportant, 9=important): 
nPriority (0=low, 9=high): 
nDifficulty (0=easy, 9=hard): 
nText-no: 
")
  (initiate-get-text-stat 'track 'lyskom-bugtrack-create text-no
			  summary severity priority difficulty))

(defun lyskom-bugtrack-create (text-stat summary severity priority difficulty)
  (if text-stat
      (initiate-get-conf-stat 'track 'lyskom-bugtrack-create-2
			      (text-stat->author text-stat)
			      summary severity priority difficulty)
    (message "Det finns ingen s}dan text.")
    (beep)))

(defun lyskom-bugtrack-create-2 (conf-stat
				 summary severity priority difficulty)
  (if conf-stat
      (save-excursion
	(set-buffer "BUGS")
	(bugtrack-create-bug summary severity priority difficulty
			     (conf-stat->name conf-stat) "LysKOM"))
    (message "Texten har ingen f|rfattare.")
    (beep)))



	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 26-Jan  activists@joker.cs.hut.fi  43 #Problem med include-filer.
Received: from varg.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA19372; Tue, 26 Jan 1993 04:57:28 +0100
Received: by varg.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA04478; Tue, 26 Jan 93 04:57:26 +0100
          (unknown)
Date: Tue, 26 Jan 93 04:57:26 +0100
From: linus@lysator.liu.se
Message-Id: <9301260357.AA04478@varg.lysator.liu.se>
To: linux-activists@joker.cs.hut.fi
Subject: Problem med include-filer.
X-Mn-Key: GCC

*** EOOH ***
Date: Tue, 26 Jan 93 04:57:26 +0100
From: linus@lysator.liu.se
To: linux-activists@joker.cs.hut.fi
Subject: Problem med include-filer.
X-Mn-Key: GCC

F|ljande program:
================ program start.
#include <sys/types.h>
#include <sys/time.h>
#include <sys/wait.h>

main()
{
    fd_set hepp;

    FD_SET(1, &hepp);
}
================ program slut.
ger f|rutom en massa varningar {ven fel om man kompilerar med -ansi.
gcc -ansi     fil.c   -o fil
In file included from /usr/include/sys/wait.h:32, from fil.c:3:
/usr/include/gnu/types.h:114: warning: `__FD_SETSIZE' redefined
/usr/include/linux/types.h:92: warning: this is the location of the previous definition
/usr/include/gnu/types.h:117: warning: `__NFDBITS' redefined
/usr/include/linux/types.h:89: warning: this is the location of the previous definition
/usr/include/gnu/types.h:128: warning: `__FD_ZERO' redefined
/usr/include/linux/types.h:117: warning: this is the location of the previous definition
/usr/include/gnu/types.h:129: warning: `__FD_SET' redefined
/usr/include/linux/types.h:97: warning: this is the location of the previous definition
/usr/include/gnu/types.h:130: warning: `__FD_CLR' redefined
/usr/include/linux/types.h:102: warning: this is the location of the previous definition
/usr/include/gnu/types.h:131: warning: `__FD_ISSET' redefined
/usr/include/linux/types.h:110: warning: this is the location of the previous definition
fil.c: In function `main':
fil.c:9: structure has no member named `__bits'
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 26-Jan   Stensture@lysator.liu.se  19 #Email
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA21116; Tue, 26 Jan 1993 20:10:01 +0100
Date: Tue, 26 Jan 1993 20:10:01 +0100
Message-Id: <199301261910.AA21116@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Stensture@lysator.liu.se (Stensture i Svenskmud)
X-From-Mud-User: Stensture i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Email

*** EOOH ***
Date: Tue, 26 Jan 1993 20:10:01 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Stensture@lysator.liu.se (Stensture i Svenskmud)
X-From-Mud-User: Stensture i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Email

Brev från Stensture i SvenskMUD.

Tjenare !

R}kade klanta mig s} du m}ste verifiera min Email igen.

Tack p} f|rhand!

CU
..


1,,
Summary-line: 27-Jan  : aronsson@lysator.liu.se  13 #Manualsida f|r ttykom
Received: from robin.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA01531; Wed, 27 Jan 1993 00:31:37 +0100
Received: by robin.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA15175; Wed, 27 Jan 93 00:31:21 +0100
          (unknown)
Date: Wed, 27 Jan 93 00:31:21 +0100
From: linus@lysator.liu.se
Message-Id: <9301262331.AA15175@robin.lysator.liu.se>
To: aronsson@lysator.liu.se
Subject: Manualsida f|r ttykom

*** EOOH ***
Date: Wed, 27 Jan 93 00:31:21 +0100
From: linus@lysator.liu.se
To: aronsson@lysator.liu.se
Subject: Manualsida f|r ttykom

Suck, varf|r skriver du p} engelska f|r?
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 27-Jan    aronsson@lysator.liu.se  10 #Re:  Manualsida f|r ttykom
Received: from robert.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA01871; Wed, 27 Jan 1993 00:40:32 +0100
Received: by robert.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA13482; Wed, 27 Jan 93 00:40:25 +0100
          (unknown)
Date: Wed, 27 Jan 93 00:40:25 +0100
From: aronsson@lysator.liu.se
Message-Id: <9301262340.AA13482@robert.lysator.liu.se>
To: linus@lysator.liu.se
Subject: Re:  Manualsida f|r ttykom

*** EOOH ***
Date: Wed, 27 Jan 93 00:40:25 +0100
From: aronsson@lysator.liu.se
To: linus@lysator.liu.se
Subject: Re:  Manualsida f|r ttykom

Manualsidor tenderar att vara skrivna p} engelska.
Kan man-programmet kolla LANG i env-variablerna i SunOS 5?
Har Sun n}gra man-sidor p} svenska? Jag kan skriva en svensk
mansida ocks}, om det tr{nger. Och en tysk...


1,,
Summary-line: 27-Jan        Dent@lysator.liu.se  21 #Haj
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA03002; Wed, 27 Jan 1993 01:10:04 +0100
Date: Wed, 27 Jan 1993 01:10:04 +0100
Message-Id: <199301270010.AA03002@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Haj

*** EOOH ***
Date: Wed, 27 Jan 1993 01:10:04 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Haj

Brev från Dent i SvenskMUD.


haj !

Ist{llet f|r synthmusik och restaurangen tusenv{gar s} g|r nu
stensture och jag ett M|rkerslott som ligger |ster om leden som
g}r norr-s|der mellan centrala byn och Strandhamn.
Just so that you know...

 / Dent


1,,
Summary-line: 27-Jan         Tom@lysator.liu.se  33 #staven
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA23323; Wed, 27 Jan 1993 11:10:04 +0100
Date: Wed, 27 Jan 1993 11:10:04 +0100
Message-Id: <199301271010.AA23323@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Tom@lysator.liu.se (Tom i Svenskmud)
X-From-Mud-User: Tom i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: staven

*** EOOH ***
Date: Wed, 27 Jan 1993 11:10:04 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Tom@lysator.liu.se (Tom i Svenskmud)
X-From-Mud-User: Tom i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: staven

Brev från Tom i SvenskMUD.


Hej!

F|rst vill jag tacka f|r att du har skrivit brev till mig.
Men jag kunde inte l{sa den. Varf|r? Ju, f|r att jag hade inne ett
gammalt meddelande fr}n dig, och har l{st den f|rst. Sedan t{nkte jag:
Ah, detta beh|vs inte, s} skrev jag in "q" s} att fr}gan komm:
"Till}tet att radera meddelanden?", s} jag t{nkte att den bara raderar 
den f|rsta som jag jusst har l{st. MEN den raderade ut b}da tv}!
Allts}, det skylle vara bra on n}gon {ndrar detta s} att fr}gan kommer 
mer specifierad.
Allts} kunde jag inte l{sa din brev om staven. D{rf|r skylle det vara bra   88[D[D[D[D 
bra om du skriver ner en g}ng till varf|r jag inte kan skaffa mig en
stav typ Fizbans stav.
En fr}ga till.
Hur kan man {ndra den texten som kommer fram n{r man skriver "titta p}
slottet" allts} min slott. Kan du skriva n{r precis allt vad
jag ska g|ra, vart jag ska "logga in" och hur. Kanske du vet hur sv}rt
det {r n{r man b|rjar programmera. Det {r de grundl{ggande sakerna som
{r det viktigaste, och det {r dessa som jag tyv{rr inte vet.

  Tom[A[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[C[A....[B[B[B[B[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D[D


1,,
Summary-line: 27-Jan        G|te@lysator.liu.se  15 #Kan ej logga in
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA02116; Wed, 27 Jan 1993 15:10:02 +0100
Date: Wed, 27 Jan 1993 15:10:02 +0100
Message-Id: <199301271410.AA02116@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: G|te@lysator.liu.se (G|te i Svenskmud)
X-From-Mud-User: G|te i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Kan ej logga in

*** EOOH ***
Date: Wed, 27 Jan 1993 15:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: G|te@lysator.liu.se (G|te i Svenskmud)
X-From-Mud-User: G|te i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Kan ej logga in

Brev från G|te i SvenskMUD.

Hej Linus, jag kan inte logga in varken direkt till muddet eller vi ftp.
Just nu har jag loggat in som g|te, men om jag f|rs|ker logga p} som uno f}r jag besked om att jag {r ny spelare och ett nytt l|senord efterfr}gas.
Jag har inte v}gat prova mig p} att ge ett nytt l|senord av r{dsla f|r att det skulle radera min gamla figur.
Som information kan jag ju n{mna att det fungerade normalt vid inloggninf|rmiddags.             Uno, alias e2nisv7@etek.chalmers.se


1,,
Summary-line: 27-Jan        Dent@lysator.liu.se  25 #Direktory
Received: by lysator.liu.se (ALPHA-6.8/6.1) id AA13785; Wed, 27 Jan 1993 20:10:03 +0100
Date: Wed, 27 Jan 1993 20:10:03 +0100
Message-Id: <199301271910.AA13785@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Direktory

*** EOOH ***
Date: Wed, 27 Jan 1993 20:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Direktory

Brev från Dent i SvenskMUD.


Haj !

Stensture och jag h}ller p} att bygga ett stort m|rkerslott |ster om
leden norr-s|der mellan strandshamn och Mudbyn...
Vi alternerar just nu placeringen av rummen (i olika dirs allts})
f|r att om en {r ute ska den andra kunna uppdatera och ladda och prova rumm
{nd}...
vi undrar nu om man kan ge access till varandras direktory p} n}got
s{tt eller om man m|jligen kan f} ett gemensamt direktory utanf|r
v}ra vanliga /spelare/<namn>, typ en tredje spelare eller n}t...

mvh Micke / Dent


1,,
Summary-line: 27-Jan  : aronsson@lysator.liu.se  13 #Skriv i ChangeLog
Received: from robin.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA17549; Wed, 27 Jan 1993 21:46:20 +0100
Received: by robin.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA17361; Wed, 27 Jan 93 21:45:57 +0100
          (unknown)
Date: Wed, 27 Jan 93 21:45:57 +0100
From: linus@lysator.liu.se
Message-Id: <9301272045.AA17361@robin.lysator.liu.se>
To: aronsson@lysator.liu.se
Subject: Skriv i ChangeLog

*** EOOH ***
Date: Wed, 27 Jan 93 21:45:57 +0100
From: linus@lysator.liu.se
To: aronsson@lysator.liu.se
Subject: Skriv i ChangeLog

Skriv i ChangeLoggen n{r du g|r n}gonting med tty-klienten.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Summary-line: 27-Jan       lenst@lysator.liu.se  44 #[ange@hplb.hpl.hp.com: Re: Ange-ftp problem with dired compress and uncompress ]
Received: from robert.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA19854; Wed, 27 Jan 1993 22:45:27 +0100
Received: by robert.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA17157; Wed, 27 Jan 93 22:45:24 +0100
          (unknown)
Date: Wed, 27 Jan 93 22:45:24 +0100
From: lenst@lysator.liu.se
Message-Id: <9301272145.AA17157@robert.lysator.liu.se>
To: linus@lysator.liu.se
Subject: [ange@hplb.hpl.hp.com: Re: Ange-ftp problem with dired compress and uncompress ]

*** EOOH ***
Date: Wed, 27 Jan 93 22:45:24 +0100
From: lenst@lysator.liu.se
To: linus@lysator.liu.se
Subject: [ange@hplb.hpl.hp.com: Re: Ange-ftp problem with dired compress and uncompress ]

Både ange-ftp och dired behöver uppdateras.  Jag fick följande svar från ange:

To: lenst@lysator.liu.se
Subject: Re: Ange-ftp problem with dired compress and uncompress 
In-Reply-To: Your message of Sun, 24 Jan 93 18:45:24 GMT.
             <9301241745.AA09944@ruben.lysator.liu.se> 
Date: Wed, 27 Jan 93 01:06:40 GMT
From: Andy Norman <ange@hplb.hpl.hp.com>

Hi Lennart.

Recently you wrote:

> (possible) Bug:
> Compress and Uncompress in dired does not work after loading ange-ftp.

> The ange-ftp version is
> ;; File:         ange-ftp.el
> ;; RCS:          $Header: /cvsroot/lyskom-elisp-client/lyskom-elisp-client/misc/check-languages.el,v 35.1 1993-05-02 21:53:37 linus Exp $

Latest version is 4.20.  Please trash your copy and upgrade.

> And the dired version is
> ;; DIRED commands for Emacs.  $Revision: 35.1 $

Ancient.  Latest version is 6.?.  Please trash your copy and upgrade.

As you have noticed, ange-ftp calls the dired routines with arguments
reversed.  This is because the next version of tree dired did indeed reverse
those arguments.

					-- ange -- <><

					ange@hplb.hpl.hp.com






1,,
Summary-line: 28-Jan   to: lenst@lysator.liu.se  17 #[ange@hplb.hpl.hp.com: Re: Ange-ftp problem with dired compress and uncompress ]
Received: from robin.lysator.liu.se by lysator.liu.se (ALPHA-6.8/6.1) id AA25753; Thu, 28 Jan 1993 01:07:13 +0100
Received: by robin.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA19255; Thu, 28 Jan 93 01:06:53 +0100
          (unknown)
Date: Thu, 28 Jan 93 01:06:53 +0100
From: linus@lysator.liu.se
Message-Id: <9301280006.AA19255@robin.lysator.liu.se>
To: lenst@lysator.liu.se
In-Reply-To: lenst@lysator.liu.se's message of Wed, 27 Jan 93 22:45:24 +0100 <9301272145.AA17157@robert.lysator.liu.se>
Subject: [ange@hplb.hpl.hp.com: Re: Ange-ftp problem with dired compress and uncompress ]

*** EOOH ***
Date: Thu, 28 Jan 93 01:06:53 +0100
From: linus@lysator.liu.se
To: lenst@lysator.liu.se
In-Reply-To: lenst@lysator.liu.se's message of Wed, 27 Jan 93 22:45:24 +0100 <9301272145.AA17157@robert.lysator.liu.se>
Subject: [ange@hplb.hpl.hp.com: Re: Ange-ftp problem with dired compress and uncompress ]

Jag hittar inte DIRED 6.? i v}r lispdir-fil (den som jag installerade
fr}n ditt dir. Du kan ta bort den nu om du inte redan gjort det.).

ange-ftp {r klart.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 29-Jan           marsj@ida.liu.se  30 #forwarded message from Martin Sjolin
Received: from curofix.ida.liu.se by lysator.liu.se (ALPHA-6.12/6.1) id AA03643; Fri, 29 Jan 1993 08:49:46 +0100
Received: from obelix by ida.liu.se (5.65b/ida.minimaster-V1.0b6d5)
	id AA06418; Fri, 29 Jan 93 08:49:40 +0100
From: Martin Sjolin <marsj@ida.liu.se>
Received: from obel27 by obelix (5.65b/ida.slave-V1.0b3)
	id AA01258; Fri, 29 Jan 93 08:49:39 +0100
Received: by obel27 (5.65b/ida.slave-V1.0b3)
	id AA00783; Fri, 29 Jan 93 08:49:38 +0100
Date: Fri, 29 Jan 93 08:49:38 +0100
Message-Id: <9301290749.AA00783@obel27>
To: Linus Tolke <linus@lysator.liu.se>
Subject: forwarded message from Martin Sjolin

*** EOOH ***
From: Martin Sjolin <marsj@ida.liu.se>
Date: Fri, 29 Jan 93 08:49:38 +0100
To: Linus Tolke <linus@lysator.liu.se>
Subject: forwarded message from Martin Sjolin

------- Start of forwarded message -------
Status: RO
Message-Id: <9301290635.AA00346@obel27>
In-Reply-To: <9301281746.AA02626@obel32>
References: <9301281746.AA02626@obel32>
From: Martin Sjolin <marsj>
To: Joakim Malmen <joama>
Cc: Jonas Wallgren <jwc>
Subject: AmigaLispen på kursbiblioteket
Date: Fri, 29 Jan 93 07:35:02 +0100

Joakim Malmen writes:
 > 
 > Varför är den läskyddad?
 > 
 > /Joakim

nja, pink & c92* kan läsa den, därför att de skulle betatesta
den. Jag skall ta kontakt med c92* & utvecklaren/portaren (håller på) 
och kontrollera ifall vi kan släppa skyddet, dvs utöka betatestarnas
skall till d92*

msj
------- End of forwarded message -------


1,,
Summary-line: 29-Jan  to: sojge@Minsk.DoCS.UU.S  34 #Signal fr}n processen. 256
Received: from robin.lysator.liu.se by lysator.liu.se (ALPHA-6.12/6.1) id AA05260; Fri, 29 Jan 1993 09:40:50 +0100
Received: by robin.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA22208; Fri, 29 Jan 93 09:40:26 +0100
          (unknown)
Date: Fri, 29 Jan 93 09:40:26 +0100
From: linus@lysator.liu.se
Message-Id: <9301290840.AA22208@robin.lysator.liu.se>
To: sojge@Minsk.DoCS.UU.SE
Cc: bug-lyskom@lysator.liu.se
In-Reply-To: Klaus Zeuge's message of Thu, 28 Jan 93 02:34:23 +0100 <9301280134.AA19332@Minsk.DoCS.UU.SE>
Subject: Signal fr}n processen. 256

*** EOOH ***
Date: Fri, 29 Jan 93 09:40:26 +0100
From: linus@lysator.liu.se
To: sojge@Minsk.DoCS.UU.SE
Cc: bug-lyskom@lysator.liu.se
In-Reply-To: Klaus Zeuge's message of Thu, 28 Jan 93 02:34:23 +0100 <9301280134.AA19332@Minsk.DoCS.UU.SE>
Subject: Signal fr}n processen. 256

Du har rekat ut fvr att kommunikationen mellan servern och din emacs
har gett ner. Det kan vara olika anledningar till det, servern kan ha
gett ner, din fvrbindelse kan ha gett ner.

Anledningen till att du fer exit code 256 dr fvr mig okdnd. Det kan ha
att gvra med att du inte har en verklig process utan bara en network
stream se att emacsen inte gvr wait eller och inte fer negon vettig
status fvr den processen.

Om du tittar i sidan Sentinels i elisp-manualen se ster det bara:
... The process sentinel is
also called if the process exits.  The sentinel receives two
arguments: the process for which the event occurred, and a string
describing the type of event.

  The string describing the event looks like one of the following:

   * `"finished\n"'.

   * `"exited abnormally with code EXITCODE\n"'.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 29-Jan        Dent@lysator.liu.se  28 #re: Luckan
Received: by lysator.liu.se (ALPHA-6.12/6.1) id AA08250; Fri, 29 Jan 1993 11:10:03 +0100
Date: Fri, 29 Jan 1993 11:10:03 +0100
Message-Id: <199301291010.AA08250@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: re: Luckan

*** EOOH ***
Date: Fri, 29 Jan 1993 11:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: re: Luckan

Brev från Dent i SvenskMUD.


long_desc {ndras inte helt enkelt. 

Det jag har gjort {r en lucka som man kan |ppna och som st{ngs
av sig sj{lv efter ett tag. luckan var egentligen bara en variabel
i rummet den var, tills jag kom p} att man m}ste ju kunna unders|ka
luckan ocks}, och d} b|r det ju st} om den {r |ppen eller inte...
S} d} gjorde jag ett objekt men set_short(0) men med namnet luckan
s} att man kunde unders|ka den. Det funkade ju fint, men sen n{r
n}gon |ppna luckan s} skall ju long_desc f|r n{mnda objekt {ndras
till '... Luckan {r |ppen', och det {r det som jag inte f}r att
funka.
Filen heter /spelare/dent/saker/lucka.c och rummet /spelare/dent/
rum/bratesal.c

mvh Dent


1,,
Summary-line: 30-Jan         Duz@lysator.liu.se  21 #edvards fick exp av mig
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA26481; Sat, 30 Jan 1993 23:10:01 +0100  (IDENT: svmud@lysator.liu.se)
Date: Sat, 30 Jan 1993 23:10:01 +0100
Message-Id: <199301302210.AA26481@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Duz@lysator.liu.se (Duz i Svenskmud)
Reply-To: t92ap@hh.se (Duz i Svenskmud)
X-From-Mud-User: Duz i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: edvards fick exp av mig

*** EOOH ***
Date: Sat, 30 Jan 1993 23:10:01 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Duz@lysator.liu.se (Duz i Svenskmud)
Reply-To: t92ap@hh.se (Duz i Svenskmud)
X-From-Mud-User: Duz i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: edvards fick exp av mig

Brev från Duz i SvenskMUD.

Det var så att edvard påstod att han inte längre fick några exp för
de monster han dödade....Jag trodde inte honom men tittade på när han
dödade några och såg på hans status att det faktiskt var sant....
sa jag tillsammans med Dent(vi var de enda magiker inne då) bestämde 
oss för att ge honom ersättning för de monster han dödat när vi såg på.
(frö+balder)=13333*2=26666xp....(Dent är mitt vittne). Hoppas det är
ok och att det fixat sig tills nästa gång han loggar in!

Duz 


1,,
Summary-line: 31-Jan        Dent@lysator.liu.se  22 #/doc/exempel
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA01015; Sun, 31 Jan 1993 01:10:02 +0100  (IDENT: svmud@lysator.liu.se)
Date: Sun, 31 Jan 1993 01:10:02 +0100
Message-Id: <199301310010.AA01015@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: /doc/exempel

*** EOOH ***
Date: Sun, 31 Jan 1993 01:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: /doc/exempel

Brev från Dent i SvenskMUD.


Haj...

Jag uppt{ckte en liten sak... Matst{lle.c i exempel {r inte
s{rskilt bra kod... Faktiskt helt felaktig kod.
Det var nalle som uppt{ckte att det var n}got konstigt
och jag har inte kollat igenom koden helt, men iallafall
s} {r viktkollar-rutinen helt bakv{nd...

 / dent


1,,
Summary-line: 30-Jan        Gäst@lysator.liu.se  20 #Saknad toke.o
Received: by lysator.liu.se (ALPHA-6.12/6.1) id AA08350; Sat, 30 Jan 1993 00:10:04 +0100
Date: Sat, 30 Jan 1993 00:10:04 +0100
Message-Id: <199301292310.AA08350@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gäst@lysator.liu.se (Gäst i Svenskmud)
X-From-Mud-User: Gäst i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Saknad toke.o

*** EOOH ***
Date: Sat, 30 Jan 1993 00:10:04 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gäst@lysator.liu.se (Gäst i Svenskmud)
X-From-Mud-User: Gäst i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Saknad toke.o

Brev från Gäst i SvenskMUD.

Hej Linus!
Detta ar Mikael, alltsa Toke.
Min objektfil har forsvunnit....... Men biblioteken finns kvar.
Hoppas att detta kan fixas.
Min emailadress ar kanske om ett par dagar m92maa@bellatrix.tdb.uu.se,
Om den inte funkar sa kan jag troligen nas pa
m92pja@bellatrix.tdb.uu.se, vilet alltsa inte ar mitt konto.

Hej D}  |nskar Toke


1,,
Summary-line: 30-Jan      Gunnar@lysator.liu.se  28 #Post och lite annat i Strandhamn.
Received: by lysator.liu.se (ALPHA-6.12/6.1) id AA04823; Sat, 30 Jan 1993 13:10:02 +0100
Date: Sat, 30 Jan 1993 13:10:02 +0100
Message-Id: <199301301210.AA04823@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Post och lite annat i Strandhamn.

*** EOOH ***
Date: Sat, 30 Jan 1993 13:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Post och lite annat i Strandhamn.

Brev från Gunnar i SvenskMUD.

Jag h}ller p} att inr{tta lite social service i Strandhamn. Jag b|rjade
med att kopiera motsvarande saker fr}n Muddevalla. N}r jag sedan
b|rjade unders|ka filerna blev jag lite fundersam.
Posten har jag till exempel inte deb blekaste aning om hur den fungerar
eller hur man l{mpligen etablerar fler postkontor.
Men {ven en mindre komplex sak som dventyrarnas klubb verkar ju on|dig
att ha i tv} upplagor.
Min ide {r att man kanske skulle kunna placera de klubbspecifika sakerna,
som avancera och lista uppdrag, i ett generiskt gillesobjekt. Detta kan
sedan {rvas av varje specifikt gilles-/klubbrum som kan komplettera med
beskrivningar, utg}ngar och speciella saker.
Detsamma g{ller naturligtvis f|r kyrkan, posten, aff{ren, pastors-
expeditionen och kanske pubar/matst{llen.

Vad tycker du?
/Gunnar


1,,
Summary-line: 30-Jan        Dent@lysator.liu.se  29 #
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA13098; Sat, 30 Jan 1993 17:10:03 +0100  (IDENT: svmud@lysator.liu.se)
Date: Sat, 30 Jan 1993 17:10:03 +0100
Message-Id: <199301301610.AA13098@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: 

*** EOOH ***
Date: Sat, 30 Jan 1993 17:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: 

Brev från Dent i SvenskMUD.


Haj !

Urs{kta att jag 'buggar' dig med mina fr}gor men jag har ingen
aning vad detta {r:
/spelare/dent/monster/amoba.c {r ett monster...
N{r jag l{t zune (min lvl1:are) sl} p} det monstret (eller
tv{rtom egentligen, men det {r v{l typ samma sak...) s} stod
det  1. en slemmig am|ba har ingen lust att anfalla dent.
  (ok, s} ska det vara man ska det verkligen st} det f|r
   de som ej {r magiker ?)
     2. Du kan inte sl}ss med statyer!
  (Och d} hade am|ban redan gett zune en smocka, men sen s} slutade
   den...)
va?

mvh Micke/dent


1, answered,,
Summary-line: 30-Jan    pawna@remus.rutgers.edu  16 #hey..
Received: from remus.rutgers.edu by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA17500; Sat, 30 Jan 1993 19:18:38 +0100 
Received: by remus.rutgers.edu (5.59/SMI4.0/RU1.5/3.08) 
	id AA24953; Sat, 30 Jan 93 13:18:35 EST
Date: Sat, 30 Jan 93 13:18:35 EST
From: pawna@remus.rutgers.edu
Message-Id: <9301301818.AA24953@remus.rutgers.edu>
To: linus@lysator.liu.se
Subject: hey..

*** EOOH ***
Date: Sat, 30 Jan 93 13:18:35 EST
From: pawna@remus.rutgers.edu
To: linus@lysator.liu.se
Subject: hey..

how come you have different address and telephone number for summer and winter break? 

so did you go to bar ? ;)
and are you going to pursue relationship further? ;) *yes I am curious! * so tell me!

:)

what's new? ;)

enjoy! 


1, answered,,
Summary-line: 31-Jan            ceder@signum.se  37 #occur-mode-goto-occurrence
Received: from mail.swip.net by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA06399; Sun, 31 Jan 1993 18:49:03 +0100 
Received: by mail.swip.net (5.65c8/1.2)
	id AA08164; Sun, 31 Jan 1993 18:49:01 +0100
Received: by signum.se (4.1/SMI-4.1)
	id AA05646; Sun, 31 Jan 93 18:20:54 +0100
Date: Sun, 31 Jan 93 18:20:54 +0100
From: ceder@signum.se (Per Cederqvist)
Message-Id: <9301311720.AA05646@signum.se>
To: linus@lysator.liu.se
Reply-To: lysator.liu.se!ceder@signum.se
Subject: occur-mode-goto-occurrence

*** EOOH ***
Date: Sun, 31 Jan 93 18:20:54 +0100
From: ceder@signum.se (Per Cederqvist)
To: linus@lysator.liu.se
Reply-To: lysator.liu.se!ceder@signum.se
Subject: occur-mode-goto-occurrence

Innan jag skickar en buggrapport till bug-gnu-emacs t{nkte jag testa
f|ljande tanke p} dig.

Tycker du som jag, att occur-mode-goto-occurrence borde g|ra
(push-mark) innan den flyttar point i den buffert man gjort M-x occur
i? Just nu k|r jag med 

(defun occur-mode-goto-occurrence ()
  "Go to the line this occurrence was found in, in the buffer it was found in."
  (interactive)
  (if (or (null occur-buffer)
	  (null (buffer-name occur-buffer)))
      (progn
	(setq occur-buffer nil
	      occur-pos-list nil)
	(error "Buffer in which occurences were found is deleted.")))
  (let* ((occur-number (/ (1- (count-lines (point-min) (point)))
			  (cond ((< occur-nlines 0)
				 (- 2 occur-nlines))
				((> occur-nlines 0)
				 (+ 2 (* 2 occur-nlines)))
				(t 1))))
	 (pos (nth occur-number occur-pos-list)))
    (pop-to-buffer occur-buffer)
    (push-mark)  ; Denna rad lade jag till. /ceder ++++++++++++++++++++
    (goto-char (marker-position pos))))

i st{llet f|r den definition som finns i replace.el.

					/ceder


1,,
Summary-line: 31-Jan        Dent@lysator.liu.se  29 #/spelare/dent/saker/darksword1.c
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA08656; Sun, 31 Jan 1993 21:10:02 +0100  (IDENT: svmud@lysator.liu.se)
Date: Sun, 31 Jan 1993 21:10:02 +0100
Message-Id: <199301312010.AA08656@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: /spelare/dent/saker/darksword1.c

*** EOOH ***
Date: Sun, 31 Jan 1993 21:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: /spelare/dent/saker/darksword1.c

Brev från Dent i SvenskMUD.


Haj !

Jag har ett allvarligt problem...
Det g}r inte att sl{ppa ovanst}ende vapen !
Det blir fel i weapon.c (stop_wielding-funktionen)...
Jag fattar ingenting.
fast... Jo. Jag vet nu vad som var problemet...
Jag gick in en ett monster och tog sv{rdet som monstret hade
wieldad... han blev wielded_by-tomte. Ok. nu vet jag.
Egentligen skulle jag sl{nga detta brev om jag bara visste HUR (med
min dumma terminal...) inte ctrl-q utan vad} ?
jaja...

urs{kta detta brev,

     / dent


1, answered,,
Summary-line:  1-Feb                      gerca  13 #
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA10548; Mon, 1 Feb 1993 01:40:46 +0100  (IDENT: gerca@lysator.liu.se)
Date: Mon, 1 Feb 1993 01:40:46 +0100
From: Gert Carlsson <gerca>
Message-Id: <199302010040.AA10548@lysator.liu.se>
Apparently-To: linus

*** EOOH ***
Date: Mon, 1 Feb 1993 01:40:46 +0100
From: Gert Carlsson <gerca>
Apparently-To: linus

Hej Linus !
Om jag inte missminner mig aer du tekniker paa Radio Ryd.
Jag har en kompis, SM7FXC Bo, som aer ordfoerande i Soelvesborgs
Naerradiofoerening i Blekinge. Jag pratade med honom under julhelgen
och han naemnde att de hade planer paa stereosaendningar och att de
behoevde en reportagelaenk. Har du tips om var man kan inhandla utrustning
av god kvalitet till humana priser vore jag tacksam om du hoer av dig.
Gert


1,,
Summary-line:  1-Feb   to: gerca@lysator.liu.se  36 #
Received: from rune.lysator.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA10581; Mon, 1 Feb 1993 01:49:57 +0100 
Received: by rune.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA13712; Mon, 1 Feb 93 01:50:08 +0100
          (unknown)
Date: Mon, 1 Feb 93 01:50:08 +0100
From: linus@lysator.liu.se
Message-Id: <9302010050.AA13712@rune.lysator.liu.se>
To: gerca@lysator.liu.se
In-Reply-To: Gert Carlsson's message of Mon, 1 Feb 1993 01:40:46 +0100 <199302010040.AA10548@lysator.liu.se>

*** EOOH ***
Date: Mon, 1 Feb 93 01:50:08 +0100
From: linus@lysator.liu.se
To: gerca@lysator.liu.se
In-Reply-To: Gert Carlsson's message of Mon, 1 Feb 1993 01:40:46 +0100 <199302010040.AA10548@lysator.liu.se>

   Date: Mon, 1 Feb 1993 01:40:46 +0100
   From: Gert Carlsson <gerca>
   Apparently-To: linus

   Hej Linus !
   Om jag inte missminner mig aer du tekniker paa Radio Ryd.
R{tt.
   Jag har en kompis, SM7FXC Bo, som aer ordfoerande i Soelvesborgs
   Naerradiofoerening i Blekinge. Jag pratade med honom under julhelgen
   och han naemnde att de hade planer paa stereosaendningar och att de
   behoevde en reportagelaenk. Har du tips om var man kan inhandla utrustning
   av god kvalitet till humana priser vore jag tacksam om du hoer av dig.
   Gert
Jag har inga tips. Link|pings n{rradiof|rening {ger en
Reportageutrustning som best}r av ryggs{ck med s{ndare och mikrofon
(kallad mes), mottagare i s{rskilld l}da.

Jag har ingen aning om var den har inhandlats eller till vilket pris.

Jag skulle misst{nka att Bengt-Olov (SM3TGR) som ocks} {r tekniker p}
Radio Ryd vet b{ttre annars kan du nog h{nvisa din kompis till Svenska
Kyrkans radio i Norrk|ping, de har egen utrustning, som vi har hyrt
n}gon g}ng, och d{rf|r tror jag att de har b{ttre koll p} var man kan
k|pa s}dan.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  1-Feb  to: pawna@remus.rutgers.e  31 #hey..
Received: from rune.lysator.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA12333; Mon, 1 Feb 1993 08:12:57 +0100 
Received: by rune.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA14882; Mon, 1 Feb 93 08:13:08 +0100
          (unknown)
Date: Mon, 1 Feb 93 08:13:08 +0100
From: linus@lysator.liu.se
Message-Id: <9302010713.AA14882@rune.lysator.liu.se>
To: pawna@remus.rutgers.edu
In-Reply-To: pawna@remus.rutgers.edu's message of Sat, 30 Jan 93 13:18:35 EST <9301301818.AA24953@remus.rutgers.edu>
Subject: hey..

*** EOOH ***
Date: Mon, 1 Feb 93 08:13:08 +0100
From: linus@lysator.liu.se
To: pawna@remus.rutgers.edu
In-Reply-To: pawna@remus.rutgers.edu's message of Sat, 30 Jan 93 13:18:35 EST <9301301818.AA24953@remus.rutgers.edu>
Subject: hey..

   Date: Sat, 30 Jan 93 13:18:35 EST
   From: pawna@remus.rutgers.edu

   how come you have different address and telephone number for summer and winter break? 
I do? I was not sure how to label the other adress. Perhaps
alternative adress would be better.

   so did you go to bar ? ;)
I was aiming to but there was a queue, and I wasn't in the mood for
queueing. I went there saturday night instead with some other friends.

   and are you going to pursue relationship further? ;) *yes I am curious! * so tell me!
Well I think that we will continue too be very good friends.
Did I tell you that I am practising Jitterbug? She is my dancing
partner.

   what's new? ;)
Nooothing.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Summary-line:  1-Feb                     moment  10 #svmud
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA15649; Mon, 1 Feb 1993 12:55:31 +0100  (IDENT: moment@lysator.liu.se)
Date: Mon, 1 Feb 1993 12:55:31 +0100
From: Martin Gauffin <moment>
Message-Id: <199302011155.AA15649@lysator.liu.se>
To: linus
Subject: svmud

*** EOOH ***
Date: Mon, 1 Feb 1993 12:55:31 +0100
From: Martin Gauffin <moment>
To: linus
Subject: svmud

Hm, urs{kta, jag trodde han (jarghard) som r}kade ut f|r objektet som skrev
random infantila fraser var en nyd|d...till dess att jag kollade.
Att en skylt sedan kom p} villov{gar...hm.
Raderad nu, i alla fall.


1, answered,,
Summary-line:  1-Feb    pawna@remus.rutgers.edu  61 #Re:  hey..
Received: from remus.rutgers.edu by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA22986; Mon, 1 Feb 1993 16:15:49 +0100 
Received: by remus.rutgers.edu (5.59/SMI4.0/RU1.5/3.08) 
	id AA15632; Mon, 1 Feb 93 10:15:04 EST
Date: Mon, 1 Feb 93 10:15:04 EST
From: pawna@remus.rutgers.edu
Message-Id: <9302011515.AA15632@remus.rutgers.edu>
To: linus@lysator.liu.se
Subject: Re:  hey..

*** EOOH ***
Date: Mon, 1 Feb 93 10:15:04 EST
From: pawna@remus.rutgers.edu
To: linus@lysator.liu.se
Subject: Re:  hey..

	From linus@lysator.liu.se Mon Feb  1 02:13:02 1993
	Received: from lysator.liu.se by remus.rutgers.edu (5.59/SMI4.0/RU1.5/3.08) 
		id AA04766; Mon, 1 Feb 93 02:13:00 EST
	Received: from rune.lysator.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA12333; Mon, 1 Feb 1993 08:12:57 +0100 
	Received: by rune.lysator.liu.se 
	          (4.1/1.34/Lysator-3.1) id AA14882; Mon, 1 Feb 93 08:13:08 +0100
	          (unknown)
	Date: Mon, 1 Feb 93 08:13:08 +0100
	From: linus@lysator.liu.se
	Message-Id: <9302010713.AA14882@rune.lysator.liu.se>
	To: pawna@remus.rutgers.edu
	In-Reply-To: pawna@remus.rutgers.edu's message of Sat, 30 Jan 93 13:18:35 EST <9301301818.AA24953@remus.rutgers.edu>
	Subject: hey..
	Status: R

	   Date: Sat, 30 Jan 93 13:18:35 EST
	   From: pawna@remus.rutgers.edu

	   how come you have different address and telephone number for summer and winter break? 

	I do? I was not sure how to label the other adress. Perhaps
	alternative adress would be better.

Well if that would be your permanent address than yes it would look
good. :) or better. :)


	   so did you go to bar ? ;)
	I was aiming to but there was a queue, and I wasn't in the mood for
	queueing. I went there saturday night instead with some other friends.

and did you buy beer from her? :-)

	   and are you going to pursue relationship further? ;) *yes I am curious! * so tell me!
	Well I think that we will continue too be very good friends.

Good friends for now?  that is good to hear! yes you do have to be
good friends first. ;)  so what do you think of her? :) :)

	Did I tell you that I am practising Jitterbug? She is my dancing
	partner.

No you didn't.  What's Jitterbug? or how does one dance Jitterbug?
what is it similar to?  :-) Cool! she must be good at it ? :)

	   what's new? ;)
	Nooothing.

Now that is hard to believe!! :-)  you made a good friend that is new!
and you are not that much of a nerd who does not go anywhere but just
in front of his computer that is new! :) *giggle*   Good to hear
though. :) enjoy!


-Pawna


1,,
Summary-line:  1-Feb      Gunnar@lysator.liu.se  17 #Konstiga funktioner
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA29730; Mon, 1 Feb 1993 19:10:03 +0100  (IDENT: svmud@lysator.liu.se)
Date: Mon, 1 Feb 1993 19:10:03 +0100
Message-Id: <199302011810.AA29730@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Konstiga funktioner

*** EOOH ***
Date: Mon, 1 Feb 1993 19:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Konstiga funktioner

Brev från Gunnar i SvenskMUD.

Vad g|r egentligen funktionen prevent_look_at_inv() i kyrkan.
Ingenting {r min gissning. Men } andra sidan g|r prevent_wield() nytta
trots att jag inte kan hitta varifr}n den anropas.

Ligger magin m}h{nda i ett dirr som jag {nnu inte hittat?


1,,
Summary-line:  1-Feb      Gunnar@lysator.liu.se  15 #kyrkan igen
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA29735; Mon, 1 Feb 1993 19:10:05 +0100  (IDENT: svmud@lysator.liu.se)
Date: Mon, 1 Feb 1993 19:10:05 +0100
Message-Id: <199302011810.AA29735@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: kyrkan igen

*** EOOH ***
Date: Mon, 1 Feb 1993 19:10:05 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: kyrkan igen

Brev från Gunnar i SvenskMUD.

B|r den speltidsvisande klockan anses som generisk, eller kan man
t{nka sig kyrkor utan en s}dan? (Bibeln som fixar iso/swascii
betraktar jag som obligatorisk.)


1,,
Summary-line:  2-Feb  to: pawna@remus.rutgers.e  28 #hey..
Received: from ruben.lysator.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA15348; Tue, 2 Feb 1993 12:33:12 +0100  (IDENT: linus@ruben.lysator.liu.se)
Received: by ruben.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA10959; Tue, 2 Feb 93 12:33:02 +0100
          (unknown)
Date: Tue, 2 Feb 93 12:33:02 +0100
From: linus@lysator.liu.se
Message-Id: <9302021133.AA10959@ruben.lysator.liu.se>
To: pawna@remus.rutgers.edu
In-Reply-To: pawna@remus.rutgers.edu's message of Mon, 1 Feb 93 10:15:04 EST <9302011515.AA15632@remus.rutgers.edu>
Subject:  hey..

*** EOOH ***
Date: Tue, 2 Feb 93 12:33:02 +0100
From: linus@lysator.liu.se
To: pawna@remus.rutgers.edu
In-Reply-To: pawna@remus.rutgers.edu's message of Mon, 1 Feb 93 10:15:04 EST <9302011515.AA15632@remus.rutgers.edu>
Subject:  hey..

   and did you buy beer from her? :-)
She wasn't there on the saturday. She was at the opera.

   good friends first. ;)  so what do you think of her? :) :)

She is cute.

	   Did I tell you that I am practising Jitterbug? She is my dancing
	   partner.

   No you didn't.  What's Jitterbug? or how does one dance Jitterbug?
   what is it similar to?  :-) Cool! she must be good at it ? :)

Well its a little like bug. I don't know...
Lots of holding hands anyway. And tossing around with the girl.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  2-Feb    pawna@remus.rutgers.edu  12 #Re:  hey..
Received: from remus.rutgers.edu by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA20800; Tue, 2 Feb 1993 15:06:30 +0100 
Received: by remus.rutgers.edu (5.59/SMI4.0/RU1.5/3.08) 
	id AA24645; Tue, 2 Feb 93 09:06:27 EST
Date: Tue, 2 Feb 93 09:06:27 EST
From: pawna@remus.rutgers.edu
Message-Id: <9302021406.AA24645@remus.rutgers.edu>
To: linus@lysator.liu.se
Subject: Re:  hey..

*** EOOH ***
Date: Tue, 2 Feb 93 09:06:27 EST
From: pawna@remus.rutgers.edu
To: linus@lysator.liu.se
Subject: Re:  hey..

hehe you sure are getting lots of practice with holdin hands and tossin around girl. ;)

she is cute that is all????? :)


/Pawna


1,,
Summary-line:  2-Feb     to: pen@lysator.liu.se  25 #buggrapport mud-fixen f|r ftp-daemonen
Received: from varg.lysator.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA06614; Tue, 2 Feb 1993 22:22:33 +0100  (IDENT: linus@varg.lysator.liu.se)
Received: by varg.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA12531; Tue, 2 Feb 93 22:22:29 +0100
          (unknown)
Date: Tue, 2 Feb 93 22:22:29 +0100
From: linus@lysator.liu.se
Message-Id: <9302022122.AA12531@varg.lysator.liu.se>
To: pen@lysator.liu.se
Subject: buggrapport mud-fixen f|r ftp-daemonen

*** EOOH ***
Date: Tue, 2 Feb 93 22:22:29 +0100
From: linus@lysator.liu.se
To: pen@lysator.liu.se
Subject: buggrapport mud-fixen f|r ftp-daemonen

Nyss skulle Dent (i SvenskMUD) ftpa lite filer fr}n svenskmud.
Han gjorde:
get <file>
och fick:
Permission DEnied.

Nu fungerar det helt pl|tsligt.

Kan de bero p} att sven var nere n{r han f|rs|kte ftpa f|rsta g}ngen?
Den var antagligen det n{mligen och {r uppe nu. Jag tycker inte det
skall ha n}gon betydelse om huruvida sven {r uppe s} det {r nog en
bug. Om det beror p} n}got annat s} skulle jag g{rna vilja veta vad
det kan vara.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  4-Feb         Tom@lysator.liu.se  19 #prog.
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA20511; Thu, 4 Feb 1993 08:10:02 +0100  (IDENT: svmud@lysator.liu.se)
Date: Thu, 4 Feb 1993 08:10:02 +0100
Message-Id: <199302040710.AA20511@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Tom@lysator.liu.se (Tom i Svenskmud)
X-From-Mud-User: Tom i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: prog.

*** EOOH ***
Date: Thu, 4 Feb 1993 08:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Tom@lysator.liu.se (Tom i Svenskmud)
X-From-Mud-User: Tom i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: prog.

Brev från Tom i SvenskMUD.


hej

Jag skylle vara tacksam om du skrev ner hur jag kan g} in i en editor
och {ndra programtexten tex i programmet castle.c.

tom



1, answered,,
Summary-line:  4-Feb      Gunnar@lysator.liu.se  16 #Kramgo
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA01177; Thu, 4 Feb 1993 13:10:03 +0100  (IDENT: svmud@lysator.liu.se)
Date: Thu, 4 Feb 1993 13:10:03 +0100
Message-Id: <199302041210.AA01177@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Kramgo

*** EOOH ***
Date: Thu, 4 Feb 1993 13:10:03 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Kramgo

Brev från Gunnar i SvenskMUD.

Lisa hade lite problem att g|ra saker i svenskmud.
Lite unders|kningar visade att funktionen id i kramgo.c inte {r
s} lysande. Jag lyckades inte |vertala emacs att jag fick {ndra i den
s} jag gjorde en kopia och r{ttade den i mitt dirr.


1,,
Summary-line:  4-Feb  to: gunnar@lysator.liu.se  29 #Kramgo
Received: from robert.lysator.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA01427; Thu, 4 Feb 1993 13:17:11 +0100  (IDENT: linus@robert.lysator.liu.se)
Received: by robert.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA04418; Thu, 4 Feb 93 13:17:04 +0100
          (unknown)
Date: Thu, 4 Feb 93 13:17:04 +0100
From: linus@lysator.liu.se
Message-Id: <9302041217.AA04418@robert.lysator.liu.se>
To: gunnar@lysator.liu.se
In-Reply-To: Gunnar i Svenskmud's message of Thu, 4 Feb 1993 13:10:03 +0100 <199302041210.AA01177@lysator.liu.se>
Subject: Kramgo

*** EOOH ***
Date: Thu, 4 Feb 93 13:17:04 +0100
From: linus@lysator.liu.se
To: gunnar@lysator.liu.se
In-Reply-To: Gunnar i Svenskmud's message of Thu, 4 Feb 1993 13:10:03 +0100 <199302041210.AA01177@lysator.liu.se>
Subject: Kramgo

   Date: Thu, 4 Feb 1993 13:10:03 +0100
   X-To-Mud-User: linus i Svenskmud.
   From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
   Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
   X-From-Mud-User: Gunnar i Svenskmud.
   X-Postoffice: /rum/post@Svenskmud.lysator.liu.se

   Brev från Gunnar i SvenskMUD.

   Lisa hade lite problem att g|ra saker i svenskmud.
   Lite unders|kningar visade att funktionen id i kramgo.c inte {r
   s} lysande. Jag lyckades inte |vertala emacs att jag fick {ndra i den
   s} jag gjorde en kopia och r{ttade den i mitt dirr.

Hon sade det. Ganska klantigt av mig. Jag har gjort felet tidigare. Om
det var i liken eller n}got s}dant.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  4-Feb        Toke@lysator.liu.se  18 #]teruppst}nden Toke
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA14596; Thu, 4 Feb 1993 19:10:04 +0100  (IDENT: svmud@lysator.liu.se)
Date: Thu, 4 Feb 1993 19:10:04 +0100
Message-Id: <199302041810.AA14596@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Toke@lysator.liu.se (Toke i Svenskmud)
Reply-To: m92maa@hassaleh.tdb.uu.se (Toke i Svenskmud)
X-From-Mud-User: Toke i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: ]teruppst}nden Toke

*** EOOH ***
Date: Thu, 4 Feb 1993 19:10:04 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Toke@lysator.liu.se (Toke i Svenskmud)
Reply-To: m92maa@hassaleh.tdb.uu.se (Toke i Svenskmud)
X-From-Mud-User: Toke i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: ]teruppst}nden Toke

Brev från Toke i SvenskMUD.

Hej :)
Allt verkar funka bra. Hoppas att niv} 23 inte var n}gons misstag, men
det blev v{ldigt mycket l{ttare att hj{lpa nya trollkarlar.
Jag tycker verkligen att 3.0 utan -O {r suver{nt, p} Svenskmud i synnerhet.
Hur st{ller man om tangentbordet p} en Sun att anv{nda ISO-svenska?
                       /Toke alias Mikael ]kersund


1, forwarded,,
Summary-line:  4-Feb         Duz@lysator.liu.se  23 #Mycket buggar!!
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA16668; Thu, 4 Feb 1993 20:10:02 +0100  (IDENT: svmud@lysator.liu.se)
Date: Thu, 4 Feb 1993 20:10:02 +0100
Message-Id: <199302041910.AA16668@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Duz@lysator.liu.se (Duz i Svenskmud)
Reply-To: t92ap@hh.se (Duz i Svenskmud)
X-From-Mud-User: Duz i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Mycket buggar!!

*** EOOH ***
Date: Thu, 4 Feb 1993 20:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Duz@lysator.liu.se (Duz i Svenskmud)
Reply-To: t92ap@hh.se (Duz i Svenskmud)
X-From-Mud-User: Duz i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Mycket buggar!!

Brev från Duz i SvenskMUD.

Ännu en konstig bugg har vi upptäckt! Nämligen med tavlan mysteriet..
Jag såg på när inferno löste uppdraget 3ggr under 4timmars tid....
UTAN att tavlorna uppdaterades, ingen annan dödlig hade under den tiden
gjort anspråk på tavlorna(och löst uppdraget). Därför samrådde jag med
Dent och Garath och vi klonade fram tavlorna åt Inferno...Sen när han
gav dem till intendenten fick han inga exp. eller en set_quest!
(detta är ju ingen bugg, utan kan nog ha en mer logisk förklaring)..
iallafall så GAV JAG Inferno 20 000 exp och set_quest till tulkas......
hoppas det är ok!
M V H
/DUZ


1,,
Summary-line:  4-Feb        Dent@lysator.liu.se  22 #LPC
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA16682; Thu, 4 Feb 1993 20:10:05 +0100  (IDENT: svmud@lysator.liu.se)
Date: Thu, 4 Feb 1993 20:10:05 +0100
Message-Id: <199302041910.AA16682@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: LPC

*** EOOH ***
Date: Thu, 4 Feb 1993 20:10:05 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: LPC

Brev från Dent i SvenskMUD.


Haj !

N{r man inherit:ar ett monster och s{tter level men inte hp,
ac och wc.... skall inte de s{ttas ungef{rligen d} ?
Det brukar funka men med h|gre levels verkar de f} mesiga 
v{rden p} hp,wc och ac (tex level 20 ger hp200 (testa ger 400) wc13
(ska vara 20 (ellervaddetvad)) och ac 5 (!) (skall vara 30 (!))

 / dent


1,,
Summary-line:  5-Feb        Dent@lysator.liu.se  28 #LPC
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA25229; Fri, 5 Feb 1993 00:10:02 +0100  (IDENT: svmud@lysator.liu.se)
Date: Fri, 5 Feb 1993 00:10:02 +0100
Message-Id: <199302042310.AA25229@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: LPC

*** EOOH ***
Date: Fri, 5 Feb 1993 00:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: LPC

Brev från Dent i SvenskMUD.


Haj !

Sn{lla !!! Jag har FORTFARANDE problem med mus.c ....
Den tycks finnas i tv} upplagor p} n}got s{tt.
I rummet /spelare/dent/rum/ratrum g} gr{ver/krafsar man p}
/ i  v{ggarna s} ramlar det ner en liten mus...
(spelare/dent/saker/mus.c)
och tar man upp den s} skall variabeln 'taken' s{ttas och
d} call_out:en intr{ffar skall den inte skriva att
musen smiter in i v{ggen...
Vad {r FEL ???????

uppr|rd,

    Dent


1,,
Summary-line:  4-Feb            samuel@front.se  25 #swnet statistik f|r januari 1993
Received: from mail.swip.net by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA03908; Fri, 5 Feb 1993 04:07:06 +0100 
Received: by mail.swip.net (5.65c8/1.2)
	id AA23913; Fri, 5 Feb 1993 04:07:04 +0100
Received: by front.se (DECUS UUCP ///1.3-1/2.5/);
          Thu,  4 Feb 93 23:31:18 MET
Date: Thu,  4 Feb 93 23:31:18 MET
Message-Id: <00967A6CEE799340.23C008BA@front.se>
From: "Terminator III" <samuel@front.se>
Subject: swnet statistik f|r januari 1993
To: linus@lysator.liu.se
X-Vms-Mail-To: uucp%"linus@lysator.liu.se"

*** EOOH ***
Date: Thu,  4 Feb 93 23:31:18 MET
From: "Terminator III" <samuel@front.se>
Subject: swnet statistik f|r januari 1993
To: linus@lysator.liu.se
X-Vms-Mail-To: uucp%"linus@lysator.liu.se"

> Jag har roat mig med att summera vad som h{nt i alla swnet-grupperna
> under januari.
>
> [r de n}gon som vill ha hela materialet, hela listorna eller fr}ga
> saker s} f}r ni h|ra av er till mig.

Hej

Tyvärr måste jag påpeka att jag inte känner mig helt komfortabel med att
det förs statistik över mina inlägg på nätet, statistik som sedan postas
ut till hela världen.
Jag kan givetvis inte hindra dig från att göra detta, men om flera kommer
med samma typ av synpunkter kan du väl kanske begränsa dig till att bara
skicka statistiken till de som uttryckligen _ber_ om den ?

med vänlig hälsning

Samuel G Sirén


1,,
Summary-line:  4-Feb        liberte@cs.uiuc.edu  37 #edebug 2.9
Received: from a.cs.uiuc.edu by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA13251; Fri, 5 Feb 1993 22:11:57 +0100 
Received: by a.cs.uiuc.edu id AA00681
  (5.64+/IDA-1.3.4 for ); Fri, 5 Feb 93 10:22:01 -0600
Received: from c.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA00671
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Fri, 5 Feb 93 10:21:55 -0600
Received: from aspen.cs.uiuc.edu (ebony.cs.uiuc.edu) by c.cs.uiuc.edu with SMTP id AA00653
  (5.65c/IDA-1.3.4 for edebug@a.cs.uiuc.edu); Thu, 4 Feb 1993 16:49:43 -0600
Message-Id: <199302042249.AA00653@c.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA14119; Thu, 4 Feb 93 16:49:36 -0600
Date: Thu, 4 Feb 93 16:49:36 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug 2.9

*** EOOH ***
Date: Thu, 4 Feb 93 16:49:36 -0600
From: liberte@cs.uiuc.edu
To: edebug@cs.uiuc.edu
Subject: edebug 2.9

Edebug version 2.9 is now available via ftp from a.cs.uiuc.edu
in /tmp/edebug.tar.Z.  

There may be an old version in /pub, so don't copy that one.  After
a week, the one in /tmp may go away, and then the one in /pub will
be correct.  Let me know if you want me to just mail it to you.

Below is the list of changes relative to 2.8.

dan
-----------
Fix handling of 0 and t edebug-form-specs.

Remove loop for consecutive special specs to simplify code.

Fix [&optional specs] again.

Bug: [&rest specs] still broken.

Bug: nested definitions may have problems still - let me know.

New variable edebug-debugger holds name of debugger for errors or quit.

Unrestore edebug-buffer's window-point after edebug display.
Needed in addition to setting the buffer point
because otherwise quitting doesnt leave point as is.
But doing it causes point not to be restored other times.
Let me know if there are problems.

Fix zmacs-regions typo for lemacs.



1,,
Summary-line:  6-Feb        wing@lysator.liu.se   9 #LPC-kurs
Received: from ruben.lysator.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA18748; Sat, 6 Feb 1993 00:48:25 +0100  (IDENT: wing@ruben.lysator.liu.se)
Received: by ruben.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA29150; Sat, 6 Feb 93 00:48:03 +0100
          (unknown)
Date: Sat, 6 Feb 93 00:48:03 +0100
From: wing@lysator.liu.se
Message-Id: <9302052348.AA29150@ruben.lysator.liu.se>
To: linus@lysator.liu.se
In-Reply-To: Linus i Svenskmud's message of Sat, 6 Feb 1993 00:10:04 +0100 <199302052310.AA17459@lysator.liu.se>
Subject: LPC-kurs

*** EOOH ***
Date: Sat, 6 Feb 93 00:48:03 +0100
From: wing@lysator.liu.se
To: linus@lysator.liu.se
In-Reply-To: Linus i Svenskmud's message of Sat, 6 Feb 1993 00:10:04 +0100 <199302052310.AA17459@lysator.liu.se>
Subject: LPC-kurs

Nja, jag t{nker nog b}de tvinga folk att l{sa ett lyskomm|te, samt annonsera 
detta i de medier du n{mnde.


1,,
Summary-line:  5-Feb        liberte@cs.uiuc.edu  13 #Re: edebug 2.9
Received: from a.cs.uiuc.edu by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA19547; Sat, 6 Feb 1993 01:08:59 +0100 
Received: by a.cs.uiuc.edu id AA11755
  (5.64+/IDA-1.3.4 for ); Fri, 5 Feb 93 17:30:16 -0600
Received: from ebony.cs.uiuc.edu by a.cs.uiuc.edu with SMTP id AA11735
  (5.64+/IDA-1.3.4 for /usr/lib/sendmail -odq -oi -fliberte edebug2); Fri, 5 Feb 93 17:30:05 -0600
Message-Id: <9302052330.AA11735@a.cs.uiuc.edu>
Received: by aspen.cs.uiuc.edu
	(5.64+/15.6) id AA14566; Fri, 5 Feb 93 17:30:01 -0600
Date: Fri, 5 Feb 93 17:30:01 -0600
From: liberte@cs.uiuc.edu
To: jackr@dblues.wpd.sgi.com
Subject: Re: edebug 2.9
Cc: edebug@cs.uiuc.edu

*** EOOH ***
Date: Fri, 5 Feb 93 17:30:01 -0600
From: liberte@cs.uiuc.edu
To: jackr@dblues.wpd.sgi.com
Subject: Re: edebug 2.9
Cc: edebug@cs.uiuc.edu

		Edebug version 2.9 is now available via ftp from a.cs.uiuc.edu
		in /tmp/edebug.tar.Z.

My mail to edebug got delayed.  edebug.tar.Z is already in pub now.

dan


1,,
Summary-line:  6-Feb  or.liu.se!ceder@signum.se  25 #occur-mode-goto-occurrence
Received: from robin.lysator.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA23970; Sat, 6 Feb 1993 03:19:25 +0100  (IDENT: linus@robin.lysator.liu.se)
Received: by robin.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA10981; Sat, 6 Feb 93 03:19:06 +0100
          (unknown)
Date: Sat, 6 Feb 93 03:19:06 +0100
From: linus@lysator.liu.se
Message-Id: <9302060219.AA10981@robin.lysator.liu.se>
To: lysator.liu.se!ceder@signum.se
In-Reply-To: Per Cederqvist's message of Sun, 31 Jan 93 18:20:54 +0100 <9301311720.AA05646@signum.se>
Subject: occur-mode-goto-occurrence

*** EOOH ***
Date: Sat, 6 Feb 93 03:19:06 +0100
From: linus@lysator.liu.se
To: lysator.liu.se!ceder@signum.se
In-Reply-To: Per Cederqvist's message of Sun, 31 Jan 93 18:20:54 +0100 <9301311720.AA05646@signum.se>
Subject: occur-mode-goto-occurrence

Jag anv{nder knappt occur-moden alls. Idag var f|rsta g}ngen sedan du
skickade brevet som jag anv{nde den och det delvis f|r att du skrev
ditt brev.

Jag tror inte de olika beteendena {r s} olika, men jag tror nog att
jag f|redrar att inte f} mark satt bara f|r att man hittar en
occurence. Man kan ha mark till n}got annat.

Jag ser occur-moden mer som ett s{tt att snabbt hitta ord och uttryck
{n n}gon slags sekvensgenererare av intressanta st{llen i koden.

Jag h}ller allts} inte med. (Men jag anv{nder ju knappast den moden).
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  8-Feb      tdi9025@abacus.hgs.se  10 #Re:  Cabals monster
Received: from Abacus.HGS.SE by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA13195; Mon, 8 Feb 1993 08:21:23 +0100  (IDENT: tdi9025@Abacus.HGS.SE)
Received: by abacus.hgs.se (5.65c/1.5)
	id AA25913; Mon, 8 Feb 1993 08:21:15 +0100
Date: Mon, 8 Feb 1993 08:21:15 +0100
From: Niklas "23" Hj{rne  <tdi9025@abacus.hgs.se>
Message-Id: <199302080721.AA25913@abacus.hgs.se>
To: linus@lysator.liu.se
Subject: Re:  Cabals monster

*** EOOH ***
Date: Mon, 8 Feb 1993 08:21:15 +0100
From: Niklas "23" Hj{rne  <tdi9025@abacus.hgs.se>
To: linus@lysator.liu.se
Subject: Re:  Cabals monster

Tack det var j{ttebra, jag har b|rjat fixa s} at det skall vara l{ttare i min 
v{rd f|r nya spelare(har {ven b|rjat fundera p} ett uppdrag f|r dessa mera om det n{r jag kommit l{ngre) :-)
Mvh Gwentar den sovande
alias Niklas Hj{rne tdi9025@abacus.hgs.se


1,,
Summary-line:  8-Feb      Gunnar@lysator.liu.se  17 #sj{lvladdande objekt
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA25634; Mon, 8 Feb 1993 14:10:02 +0100  (IDENT: svmud@lysator.liu.se)
Date: Mon, 8 Feb 1993 14:10:02 +0100
Message-Id: <199302081310.AA25634@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: sj{lvladdande objekt

*** EOOH ***
Date: Mon, 8 Feb 1993 14:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: sj{lvladdande objekt

Brev från Gunnar i SvenskMUD.

Jo, jag var vid n}got tillf{lle medveten om det fenomenet.

P} Nannymud har de lyckats l|sa problemet s} att sj{lvladdande objekt
inte beh|ver laddas vid omstart. Jag vet inte hur eller om det var
knepigt, men det {r ju en betydligt b{ttre l|sning {n vi har.


1, answered,,
Summary-line: 12-Feb         martin.zurn@jrc.it  17 #
Received: from elrond.ida.liu.se by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA18283; Fri, 12 Feb 1993 14:45:14 +0100 
Received: from relay1.iunet.it by elrond.ida.liu.se with SMTP
	(5.61-bind 1.5X+ida/IDA-1.2.8-mc2.5-2) id AA14643; Fri, 12 Feb 93 14:44:57 +0100
Received: from jrc.it (jrc.jrc.it) by relay1.iunet.it with SMTP
	(5.65c8/IDA-1.2.8) id AA00772; Fri, 12 Feb 1993 14:35:02 +0100
Received: from dac.ise.jrc (dacls1) by jrc.it; Fri, 12 Feb 93 14:27:40 +0100
Received: by dac.ise.jrc (4.1/JRC-S-1.3)
	id AA10106; Fri, 12 Feb 93 14:33:06 +0100
Date: Fri, 12 Feb 93 14:33:06 +0100
From: martin.zurn@jrc.it (Martin ZURN)
Message-Id: <9302121333.AA10106@dac.ise.jrc>
Apparently-To: linus@lysator.liu.se

*** EOOH ***
Date: Fri, 12 Feb 93 14:33:06 +0100
From: martin.zurn@jrc.it (Martin ZURN)
Apparently-To: linus@lysator.liu.se

Hej paa Dej, Linus,
	Hoppas att Du maar bra. Kommer Di ihaag att vi har haft flera
cw qso naer jag var dl1gbz i Tyskland? Nu arbetar jag i Italien och
har just finnat Din adress i en kanadisk samling av e-mail adresser.

Vi hoers
Martin, dl1gbz och ik2rmz ex f0iry

Dr. Martin A. Zurn                                 Fax: (+39-332) 789156
Centro Comune di Ricerca TP 680                    Tel: (+39-332) 785346
ISEI-IE Applied Optics Lab                E-mail martin.zurn@cen.jrc.it
I-21020 Ispra (VA) Italy   (martin.zurn%cen.jrc.it@i2unix.dist.unige.it)


1,,
Summary-line: 16-Feb  mud@GOEDEL.UNI-MUENSTER.D  20 #Morgengrauen-Adresse
Received: from GOEDEL.UNI-MUENSTER.DE by lysator.liu.se  with SMTP (ALPHA-6.12/6.1/Lysator) id AA12899; Tue, 16 Feb 1993 13:15:19 +0100 
Received: from laurin.uni-muenster.de by math.uni-muenster.de (4.1/SMI-4.1/smah-1.36)
	id AA21518; Tue, 16 Feb 93 13:14:08 +0100
Organization: Westfaelische Wilhelms-Universitaet, Muenster, Germany
              Department of Mathematics
Date: Tue, 16 Feb 93 13:14:08 +0100
From: Mud Administrator <mud@GOEDEL.UNI-MUENSTER.DE>
Message-Id: <9302161214.AA21518@math.uni-muenster.de>
To: linus@lysator.liu.se
Subject: Morgengrauen-Adresse

*** EOOH ***
Organization: Westfaelische Wilhelms-Universitaet, Muenster, Germany
              Department of Mathematics
Date: Tue, 16 Feb 93 13:14:08 +0100
From: Mud Administrator <mud@GOEDEL.UNI-MUENSTER.DE>
To: linus@lysator.liu.se
Subject: Morgengrauen-Adresse

 Hi !

 Die Adresse von Morgengrauen hat sich am Freitag, den 5.2.1993 geaendert
und lautet jetzt pascal.uni-muenster.de 4711
                 128.176.121.56         4711. 
 Eigentlich sollte auf dem alten Rechner ein Programm laufen, das genau das
mitteilt, wenn man sich einloggen will, aber da ist leider einiges schief-
gelaufen, deshalb waehle ich diesen Weg der Benachrichtigung.


 Tschuess
   Jof


1,, svmud,
Summary-line: 21-Feb      Gunnar@lysator.liu.se  21 #obj/monster
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA20779; Sun, 21 Feb 1993 00:10:04 +0100  (IDENT: svmud@lysator.liu.se)
Date: Sun, 21 Feb 1993 00:10:04 +0100
Message-Id: <199302202310.AA20779@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: obj/monster

*** EOOH ***
Date: Sun, 21 Feb 1993 00:10:04 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: obj/monster

Brev från Gunnar i SvenskMUD.
Kopia till:   st

Funktionen set_level i obj/monster s{tter {ven skydds- och vapenklass.
Dock s{tts dessa v{ldigt l}gt, v{sentligt mycket l{gre {n i
doc/build/monster.lista. [r detta meningen eller {r det ett miss|de?
Har det alltid varit s}?

/Gunnar


1, forwarded, answered,, svmud,
Summary-line: 24-Feb      Edvard@lysator.liu.se  25 #museiuppdraget
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA03129; Wed, 24 Feb 1993 02:10:01 +0100  (IDENT: svmud@lysator.liu.se)
Date: Wed, 24 Feb 1993 02:10:01 +0100
Message-Id: <199302240110.AA03129@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Edvard@lysator.liu.se (Edvard i Svenskmud)
Reply-To: owestman@nyx.cs.du.edu (Edvard i Svenskmud)
X-From-Mud-User: Edvard i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: museiuppdraget

*** EOOH ***
Date: Wed, 24 Feb 1993 02:10:01 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Edvard@lysator.liu.se (Edvard i Svenskmud)
Reply-To: owestman@nyx.cs.du.edu (Edvard i Svenskmud)
X-From-Mud-User: Edvard i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: museiuppdraget

Brev från Edvard i SvenskMUD.
Kopia till:   linus

Under f|rs|k att fullborda museiuppdraget har jag funnit att det
alltid resulterar i en slutgiltig bugg. Man g|r allt r{tt, dvs 
h{mtar delarna till krukan och ger dem till intendenten,
l}ser upp skrubben och h{mtar stegen och repet och 
sedan firar man ner sig till tavlorna.
Men tavlorna {r aldrig d{r! det st}r att normalt s} finns det tv}
vackra tavlor d{r men de {r borta nu
Jag {r inte den ende som r}kat ut f|r detta.
D} jag inte vet vem som skapat uppdraget f}r du detta brev ist{llet.
                     Edvard


1, answered,, svmud,
Summary-line: 25-Feb        Dent@lysator.liu.se  36 #monster.c
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA11785; Thu, 25 Feb 1993 21:10:08 +0100  (IDENT: svmud@lysator.liu.se)
Date: Thu, 25 Feb 1993 21:10:08 +0100
Message-Id: <199302252010.AA11785@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: dent@krynn.solace.hsh.se
X-To-Mud-User: dent i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: monster.c

*** EOOH ***
Date: Thu, 25 Feb 1993 21:10:08 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: dent@krynn.solace.hsh.se
X-To-Mud-User: dent i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: monster.c

Brev från Dent i SvenskMUD.
Kopia till:   dent


Hmm... antagligen tar jag mig vatten |ver huvudet nu, men
jag har m{rkt att set_name(({"namn1","namn2","namn3"}))
inte alltid funkar...
Kan detta bero p} att /obj/monster.c inte {rver /std/namn.c ?

Jag h}ller f|rresten p} med tv} objekt, fuel.c och fuel_consumer.c, 
som jag hade t{nkt skulle bli filer som man ska {rva och kunna
l{tt g|ra generiska br{nsle-drivna saker med.
Tanken {r att olika br{nsle-objekt (skapade av olika magiker),
som {r av samma 'typ' (tex bensin, batteri, gengas eller vad som
helst...) ska kunna anv{ndas av alla objekt som drivs av just
den typen av br{nsle.
Kanske inte helt korrekt att man fyller p} visst m}nga enheter
batteri i en ficklampa, men {nd}...

[r det n}got speciellt man ska t{nka p} d} man g|r filer som
ska {rvas (jo, en hel del, men n}got som man inte t{nker p}
som relativt oerfaren magiker...) ?

mvh Micke


1,, svmud,
Summary-line: 25-Feb        Dent@lysator.liu.se  27 #fuel.c
Received: by lysator.liu.se  (ALPHA-6.12/6.1/Lysator) id AA14007; Thu, 25 Feb 1993 22:10:05 +0100  (IDENT: svmud@lysator.liu.se)
Date: Thu, 25 Feb 1993 22:10:05 +0100
Message-Id: <199302252110.AA14007@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: fuel.c

*** EOOH ***
Date: Thu, 25 Feb 1993 22:10:05 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: fuel.c

Brev från Dent i SvenskMUD.

Haj !

Jag har nu laddat ner lite filer och avlusat dem. Kolla in 
filerna /spelare/dent/Fuel
fuel.c     <--- include-fil f|r br{nslen
fuel_consumer.c     <--- include-fil f|r saker som kons.. just det.
ficklampa.c
batteri.c

Vad {r egentligen skillnaden mellan en include:ad fil och en
inherit:ad ?
En include:ad {r v{l bara som en j{ttelik #define, s} vad {r en
inherit ?

den evigt fr}gande, dent.


1,,
Summary-line: 15-Mar   to: maffeis@ifi.unizh.ch  82 #C++ symbol name unmungler wanted
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA04920; Mon, 15 Mar 1993 16:30:29 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA25331; Mon, 15 Mar 1993 16:30:25 +0100
          (unknown)
Date: Mon, 15 Mar 1993 16:30:25 +0100
From: linus@lysator.liu.se
Message-Id: <199303151530.AA25331@bodil.lysator.liu.se>
To: maffeis@ifi.unizh.ch (Silvano Maffeis)
In-Reply-To: maffeis@ifi.unizh.ch's message of Sat, 13 Mar 93 10:17:06 GMT
Subject: C++ symbol name unmungler wanted

*** EOOH ***
Date: Mon, 15 Mar 1993 16:30:25 +0100
From: linus@lysator.liu.se
To: maffeis@ifi.unizh.ch (Silvano Maffeis)
In-Reply-To: maffeis@ifi.unizh.ch's message of Sat, 13 Mar 93 10:17:06 GMT
Subject: C++ symbol name unmungler wanted

   Newsgroups: comp.lang.c++,alt.sources.wanted
   From: maffeis@ifi.unizh.ch (Silvano Maffeis)
   Date: Sat, 13 Mar 93 10:17:06 GMT

   Hello!

   I'm looking for a filter capable of unmungling g++ symbol codes
   like  bla__3objFi into the user defined symbol names.
   Any hint is appreciated.

   Please e-mail

I was looking for the opposite the other day and found just this.
There is a file in the g++-distribution with the name cp-dem.c. That
file does the converting. I wrote a simple inteface (so that I can
type the symbol-name on the command line) that I send to you so you
can get an idea of how it is working. It should be self explanatory.
The xalloc, xrealloc are required by the cp-dem.c program.
================================================================
#include <stdio.h>

extern char * cplus_demangle(char *);

main(int argc, char **argv)
{
    char * ret;

    if (argc != 2) {
	fprintf(stderr,"Wrong number of arguments.\n");
	exit(1);
    }

    ret = cplus_demangle(argv[1]);

    if (ret)
	printf("Funktionen: %s\n", ret);
    else
	printf("Failed.\n");
    
}

char *
xmalloc(int size)
{
    char * area;

    area = (char *)malloc(size);

    if (area)
	return area;

    fprintf(stderr,"Not enough memory.\n");
    exit(1);
}

char *
xrealloc(char *addr, int size)
{
    char * area;

    area = (char *)realloc(addr,size);

    if (area)
	return area;

    fprintf(stderr,"Not enough memory.\n");
    exit(1);
}
================================================================
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 15-Mar     to: martin.zurn@jrc.it  19 #hpe cu sn on cw
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA08051; Mon, 15 Mar 1993 17:33:32 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA27005; Mon, 15 Mar 1993 17:33:29 +0100
          (unknown)
Date: Mon, 15 Mar 1993 17:33:29 +0100
From: linus@lysator.liu.se
Message-Id: <199303151633.AA27005@bodil.lysator.liu.se>
To: martin.zurn@jrc.it
In-Reply-To: Martin ZURN's message of Fri, 12 Feb 93 14:33:06 +0100 <9302121333.AA10106@dac.ise.jrc>
Subject: hpe cu sn on cw

*** EOOH ***
Date: Mon, 15 Mar 1993 17:33:29 +0100
From: linus@lysator.liu.se
To: martin.zurn@jrc.it
In-Reply-To: Martin ZURN's message of Fri, 12 Feb 93 14:33:06 +0100 <9302121333.AA10106@dac.ise.jrc>
Subject: hpe cu sn on cw

Hej, hej.
Kul att h|ra av dig.

Jas}, du {r i italien, det var ju sp{nnande. Du har ju lite l{ttare
f|r att flytta runt till olika jobb, men vi svenska jobbar p} det
(1995). Ha det s} kul!
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 15-Mar       linus@lysator.liu.se  36 #Re: Are any fellow broadcasters out here?
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA12130; Mon, 15 Mar 1993 18:52:11 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA29062; Mon, 15 Mar 1993 18:52:05 +0100
          (unknown)
Date: Mon, 15 Mar 1993 18:52:05 +0100
From: linus@lysator.liu.se
Message-Id: <199303151752.AA29062@bodil.lysator.liu.se>
In-Reply-To: safrin@hubcap.clemson.edu's message of Wed, 10 Mar 1993 21:04:50 GMT
Newsgroups: rec.radio.noncomm
Subject: Re: Are any fellow broadcasters out here?
References: <1993Mar10.210450.19002@hubcap.clemson.edu>
Distribution: world
Apparently-To: <linus@lysator.liu.se>

*** EOOH ***
Date: Mon, 15 Mar 1993 18:52:05 +0100
From: linus@lysator.liu.se
In-Reply-To: safrin@hubcap.clemson.edu's message of Wed, 10 Mar 1993 21:04:50 GMT
Newsgroups: rec.radio.noncomm
Subject: Re: Are any fellow broadcasters out here?
References: <1993Mar10.210450.19002@hubcap.clemson.edu>
Distribution: world
Apparently-To: <linus@lysator.liu.se>

In article <1993Mar10.210450.19002@hubcap.clemson.edu> safrin@hubcap.clemson.edu (Steve Afrin) writes:
>Who else do we have out here?  Any other college radio
>stations subscribe to this newsgroup?

I am one of the collaborators at the Radio Ryd at Linkoeping
university, Linkoeping, Sweden, Europe. We are using the
"n{rradiolagen" to do our show. That is a swedish law that since
1977 allows non-profit-making organisations to transmit radio. This is
mostly aimed at political parties and churches but provided a loophole
for lots of radio interested people.

>I was thinking of trying to either find a group or create
>a group on Usenet where we broadcasters could share tips,
>stories, and programming and show ideas between stations.

I think that the group rec.radio.broadcasting should be used for this.
I am sorry to say that my views on this matter is not shared by the
moderator of the group. The groups has become lot wider than just
exchange of matters between broadcasters. Perhaps time for a
rec.radio.broadcasters?
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered, forwarded,,
Summary-line: 15-Mar       gt92agu@txfs1.hfb.se  28 #Datorförening
Received: from cfxfs1.hfb.se (130.238.194.5) by lysator.liu.se (ALPHA-6.36/6.16) id AA13680; Mon, 15 Mar 1993 19:21:35 +0100
Received: from t.hfb.se (txfs1.hfb.se) by cfxfs1.hfb.se with SMTP id AA05130
  (5.65c8/IDA-1.4.4 for linus@lysator.liu.se); Mon, 15 Mar 1993 19:25:09 +0100
Received: from tex2.hfb.se by t.hfb.se (4.1/HFB_T-1.1)
	id AA04694; Mon, 15 Mar 93 19:20:59 +0100
Date: Mon, 15 Mar 93 19:20:59 +0100
From: gt92agu@txfs1.hfb.se (Anders Gustavsson - HFB T gt92)
Message-Id: <9303151820.AA04694@t.hfb.se>
To: linus@lysator.liu.se
Subject: Datorförening
X-Charset: ASCII
X-Char-Esc: 29

*** EOOH ***
Date: Mon, 15 Mar 93 19:20:59 +0100
From: gt92agu@txfs1.hfb.se (Anders Gustavsson - HFB T gt92)
To: linus@lysator.liu.se
Subject: Datorförening

Hej!

Din email-adress fick jag av en kompis som sj_lv _r med i lysator och jag hoppas att det _r "r_tt person" jag fr_gar (om inte s_ kan du v_l se till att "r_tt person" f_r detta mail). 

Jag har lite fr_gor ang_ende starten av en datorf_rening. 

* Hur har Ni tiggt till Er datorer och vart/till vem man ska v_nda sig i denna fr_ga?

* Kan man ans_ka om bidrag eller dylikt f_r denna verksamhet (igentligen: hur finasierar ni f_reningen)?

* Hur har ni gjort med programvaror (f_tt av skolan, k_pt egna licenser etc)?

* Har Ni n_gra stadgar och m_tesprotokoll som Du kan mail:a upp till mig?

* Har Du n_gra _vriga tips eller r_d?

Som du f_rst_r av ovanst_ende s_ har jag dragit ig_ng en en datorf_rening h_r p_ h_gskolan Falun/Borl_nge. Jag hoppas att vi kan n_gon form av utbyte i framtiden. En g_stf_rel_sning om t ex brister i UNIX av n_gon i lysator? Beltalningen skulle givetvis vara "studentbetonad": resan betald, _vernattning hos n_gon i f_reningen och gratis _l p_ k_ren...

 _ _         __    ___  __   ____    
|* *| |\  | | *\  |    | *\ |       Anders Gustavsson, GT 1
| \ | | \ | |   ) |--  |__/  ----   University of Falun/Borl_nge SWEDEN
|\_/| |  \| |  _  |___ |  \  ____|  Email: gt92agu@t.hfb.se


1,,
Summary-line: 15-Mar  o: lysator@lysator.liu.se  39 #[gt92agu@txfs1.hfb.se: Datorfvrening]
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA13989; Mon, 15 Mar 1993 19:27:41 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA29872; Mon, 15 Mar 1993 19:27:37 +0100
          (unknown)
Date: Mon, 15 Mar 1993 19:27:37 +0100
From: linus@lysator.liu.se
Message-Id: <199303151827.AA29872@bodil.lysator.liu.se>
To: lysator@lysator.liu.se
Subject: [gt92agu@txfs1.hfb.se: Datorfvrening]

*** EOOH ***
Date: Mon, 15 Mar 1993 19:27:37 +0100
From: linus@lysator.liu.se
To: lysator@lysator.liu.se
Subject: [gt92agu@txfs1.hfb.se: Datorfvrening]

	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)
Date: Mon, 15 Mar 93 19:20:59 +0100
From: gt92agu@txfs1.hfb.se (Anders Gustavsson - HFB T gt92)
To: linus@lysator.liu.se
Subject: Datorfvrening

Hej!

Din email-adress fick jag av en kompis som sj_lv _r med i lysator och jag hoppas att det _r "r_tt person" jag fr_gar (om inte s_ kan du v_l se till att "r_tt person" f_r detta mail). 

Jag har lite fr_gor ang_ende starten av en datorf_rening. 

* Hur har Ni tiggt till Er datorer och vart/till vem man ska v_nda sig i denna fr_ga?

* Kan man ans_ka om bidrag eller dylikt f_r denna verksamhet (igentligen: hur finasierar ni f_reningen)?

* Hur har ni gjort med programvaror (f_tt av skolan, k_pt egna licenser etc)?

* Har Ni n_gra stadgar och m_tesprotokoll som Du kan mail:a upp till mig?

* Har Du n_gra _vriga tips eller r_d?

Som du f_rst_r av ovanst_ende s_ har jag dragit ig_ng en en datorf_rening h_r p_ h_gskolan Falun/Borl_nge. Jag hoppas att vi kan n_gon form av utbyte i framtiden. En g_stf_rel_sning om t ex brister i UNIX av n_gon i lysator? Beltalningen skulle givetvis vara "studentbetonad": resan betald, _vernattning hos n_gon i f_reningen och gratis _l p_ k_ren...

 _ _         __    ___  __   ____    
|* *| |\  | | *\  |    | *\ |       Anders Gustavsson, GT 1
| \ | | \ | |   ) |--  |__/  ----   University of Falun/Borl_nge SWEDEN
|\_/| |  \| |  _  |___ |  \  ____|  Email: gt92agu@t.hfb.se



1, answered,,
Summary-line: 16-Mar         martin.zurn@jrc.it  21 #
Received: from relay1.iunet.it (192.106.1.3) by lysator.liu.se (ALPHA-6.36/6.16) id AA03634; Tue, 16 Mar 1993 13:51:07 +0100
Received: from jrc.it (relay.jrc.it) by relay1.iunet.it with SMTP id AA26240
  (5.65c8/IDA-1.4.4 for <linus@lysator.liu.se>); Tue, 16 Mar 1993 08:14:38 +0100
Received: from dac.ise.jrc (dacls1) by jrc.it; Tue, 16 Mar 93 08:13:37 +0100
Received: by dac.ise.jrc (4.1/JRC-S-1.3)
	id AA19579; Tue, 16 Mar 93 08:15:22 +0100
Date: Tue, 16 Mar 93 08:15:22 +0100
From: martin.zurn@jrc.it (Martin ZURN)
Message-Id: <9303160715.AA19579@dac.ise.jrc>
Apparently-To: linus@lysator.liu.se

*** EOOH ***
Date: Tue, 16 Mar 93 08:15:22 +0100
From: martin.zurn@jrc.it (Martin ZURN)
Apparently-To: linus@lysator.liu.se

Hej paa Dej igen, Linus!
	Tack foer svaret. Ja, jag arbetar hr i IK2 i fr JRC som optik expert.
Jag er qrv med LW och Corsair. Tyvaerr finns det ej ngn klubbstn hr.
Aer Du alltjaemt med i naet tfc? Foer mej er det naestan omoegligt pga laanga
distanser mellan SM och I. Ibland goer jag qni i SAN/I om Loerdagarna (men
ofta er jag ej hemma (er gift nu, hi). Under moerkaste vinter tid kunne jag
aeven saenda qtc till 6ssk/bsk men det var ej laett. SAN/I gaar alldeles
utmaerkt foer qtc utbyte mellan SM och USA. Synd att det finns ej mera EU-Net
eller SAN/L 
	Baesta 73 till Dei och de Dina och till alla vaenner fraan sk5eu
(kaenner stn fraan test)

Dr. Martin A. Zurn                                 Fax: (+39-332) 789156
Centro Comune di Ricerca TP 680                    Tel: (+39-332) 785346
ISEI-IE Applied Optics Lab                E-mail martin.zurn@cen.jrc.it
I-21020 Ispra (VA) Italy   (martin.zurn%cen.jrc.it@i2unix.dist.unige.it)


1, answered,,
Summary-line: 16-Mar        mnhe@celsiustech.se  26 #
Received: from nobeltech.nobeltech.se (139.58.1.3) by lysator.liu.se (ALPHA-6.36/6.16) id AA03830; Tue, 16 Mar 1993 13:55:54 +0100
Received: by nobeltech.nobeltech.se (5.67a8/2.3)
	id AA19404; Tue, 16 Mar 1993 13:47:58 +0100
Date: Tue, 16 Mar 1993 13:47:58 +0100
From: Magnus Hedner  <mnhe@celsiustech.se>
Message-Id: <199303161247.AA19404@nobeltech.nobeltech.se>
Apparently-To: linus@lysator.liu.se

*** EOOH ***
Date: Tue, 16 Mar 1993 13:47:58 +0100
From: Magnus Hedner  <mnhe@celsiustech.se>
Apparently-To: linus@lysator.liu.se

Hejsan igen!

Det {r sv}rt att {gna sig }t arbetet, iallafall p} arbetstid...

Jag har en kort fr}ga: har Du n}gon bra /etc/hosts e dyl? P} min
dator finns det v{ldigt f} adresser till utlandet, dock kan man
k|ra med n{t"nummer" (vadetnuheter,finnsv{ln}gotfiffigtnamn) h{rifr}n.
Vore lycklig f|r ett majl med s}na...

Finns det kanske n}n konferens i njuvsnett som har som uppgift att
sprida nummer?

]ss} en fr}ga till som jag just kom p}. Vet du om man kan skicka majl
till folk p} memonet (t.ex. njutt, som har n}t konstigt memoid)? Jag
antar att man m}ste passera n}gon rutter e dyl...?

N{ nu ska jag inte st|ra l{ngre. Hoppas Link|ping st}r kvar. Jag h|rde
att Ni bytt lokal?! (S}g "kallelse" till ++19fest) Hoppas att det {r
lite st|rre utrymme d{r {n p} f|rra st{llet! (=Mindre nacksp{rr)

Hej d} } harebra!


1,,
Summary-line: 16-Mar     to: martin.zurn@jrc.it  21 #cw och sarnet
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA05345; Tue, 16 Mar 1993 14:38:53 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA03359; Tue, 16 Mar 1993 14:38:47 +0100
          (unknown)
Date: Tue, 16 Mar 1993 14:38:47 +0100
From: linus@lysator.liu.se
Message-Id: <199303161338.AA03359@bodil.lysator.liu.se>
To: martin.zurn@jrc.it
In-Reply-To: <9303160715.AA19579@dac.ise.jrc> (martin.zurn@jrc.it)
Subject: cw och sarnet

*** EOOH ***
Date: Tue, 16 Mar 1993 14:38:47 +0100
From: linus@lysator.liu.se
To: martin.zurn@jrc.it
In-Reply-To: <9303160715.AA19579@dac.ise.jrc> (martin.zurn@jrc.it)
Subject: cw och sarnet

Det h{nder inte s} ofta att jag {r inne i n{ten numera men det beror
inte p} sv}righeter att qni utan snarare p} att jag g|r andra saker p}
de tiderna. Det har blivit mycket sittande med datorerna sista }ret.

Jag h}ller fortfarande p} och studerar n{mligen och d} har man inte s}
mycket tid |ver till att springa till schacket, i detta fall SK5EU.

Vi ses.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 16-Mar         martin.zurn@jrc.it  14 #Re:  cw och sarnet
Received: from relay1.iunet.it (192.106.1.3) by lysator.liu.se (ALPHA-6.36/6.16) id AA06235; Tue, 16 Mar 1993 15:00:36 +0100
Received: from jrc.it (relay.jrc.it) by relay1.iunet.it with SMTP id AA01779
  (5.65c8/IDA-1.4.4 for <linus@lysator.liu.se>); Tue, 16 Mar 1993 15:00:44 +0100
Received: from dac.ise.jrc (dacls1) by jrc.it; Tue, 16 Mar 93 14:59:23 +0100
Received: by dac.ise.jrc (4.1/JRC-S-1.3)
	id AA00133; Tue, 16 Mar 93 15:01:07 +0100
Date: Tue, 16 Mar 93 15:01:07 +0100
From: martin.zurn@jrc.it (Martin ZURN)
Message-Id: <9303161401.AA00133@dac.ise.jrc>
To: linus@lysator.liu.se
Subject: Re:  cw och sarnet

*** EOOH ***
Date: Tue, 16 Mar 93 15:01:07 +0100
From: martin.zurn@jrc.it (Martin ZURN)
To: linus@lysator.liu.se
Subject: Re:  cw och sarnet

Kenner problemet, var ju oxo student fr maanga aar ...

Men derefter blir det ej bettre, det finns alltid maanga saker som man
maaste goera ...

I alla fall hoppas jag vi hoers igen ...

Ciao och hej


1,,
Summary-line: 18-Mar        gnulists@ai.mit.edu  55 #gnu.gnusenet.config:moderated groups
Received: from life.ai.mit.edu (128.52.32.80) by lysator.liu.se (ALPHA-6.36/6.16) id AA29681; Thu, 18 Mar 1993 18:27:38 +0100
Received: from raisin-nut (raisin-nut.ai.mit.edu) by life.ai.mit.edu (4.1/AI-4.10) id AA25906; Thu, 18 Mar 93 12:27:33 EST
From: gnulists@ai.mit.edu (GNU Mailing List Maintenance)
Received: by raisin-nut (4.1/AI-4.10) id AA13945; Thu, 18 Mar 93 12:27:31 EST
Date: Thu, 18 Mar 93 12:27:31 EST
Message-Id: <9303181727.AA13945@raisin-nut>
To: linus@lysator.liu.se
In-Reply-To: <9303100606.AA29525@raisin-scone> "tower@ai.mit.edu"
Reply-To: gnu@prep.ai.mit.edu
Sender: gnu@prep.ai.mit.edu
Organization: Project GNU, Free Software Foundation,
           675 Mass. Ave., Cambridge, MA  02139, USA   +1 (617) 876-3296
Subject: gnu.gnusenet.config:moderated groups

*** EOOH ***
From: gnulists@ai.mit.edu (GNU Mailing List Maintenance)
Date: Thu, 18 Mar 93 12:27:31 EST
To: linus@lysator.liu.se
In-Reply-To: <9303100606.AA29525@raisin-scone> "tower@ai.mit.edu"
Reply-To: gnu@prep.ai.mit.edu
Sender: gnu@prep.ai.mit.edu
Organization: Project GNU, Free Software Foundation,
           675 Mass. Ave., Cambridge, MA  02139, USA   +1 (617) 876-3296
Subject: gnu.gnusenet.config:moderated groups

   Path: ai-lab!enterpoop.mit.edu!biosci!uwm.edu!cs.utexas.edu!uunet!mcsun!sunic!liuida!isy!lysator.liu.se!news.lysator.liu.se!linus
   From: linus@lysator.liu.se (Linus Tolke Y)
   Newsgroups: gnu.gnusenet.config
   Subject: moderated groups
   Message-ID: <LINUS.93Mar9005923@bodil.lysator.liu.se>
   Date: 8 Mar 93 23:59:23 GMT
   Sender: news@lysator.liu.se
   Organization: Lysator Academic Computer Society, Link|ping University
   Lines: 20

   Why is it that I always forget what groups are moderated and what
   groups are not? And why is it that I always post to the moderated
   groups with Distribution: local?

   And why is it that the gnu automatic approval posting approves my
   posting and sends it all over the world (when it is just of interest
   to the local organisation, and for a vaste majority of the usenet
   community, totally unreadable)?

   My suggestion is this:
   * Make all automatic approval skripts barf when the Distribution:-line
     doesn't contain the distribution: world.

if one posts to a moderated group, one gets the default distribution
the moderator uses.  Moderators are not in the habit of checking
Distribution: headers.

   * Make gnus warn extra if the group is moderated. A little: This
     groups is moderated, are you sure you want to do this (y or n)?

good idea.  send it to the maintainer or at lest post it to
gnu.emacs.gnus.  it's possible a later version then the one you are
using already does this.

   -- 
	   /Linus
   *****	Wherever I exec my `which emacs`, is my $HOME.	*****
   Linus Tolke				SM7OUU, linus@lysator.liu.se
   Student at the				member of SK5EU LiTHSA
   Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)

thanx -len

Member, League for Programming Freedom.  Ask: lpf@uunet.uu.net 


1,, svmud,
Summary-line: 21-Mar      Gunnar@lysator.liu.se  52 #Byggande
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA18562; Sun, 21 Mar 1993 14:10:02 +0100
Date: Sun, 21 Mar 1993 14:10:02 +0100
Message-Id: <199303211310.AA18562@lysator.liu.se>
To: d92maran@und.ida.liu.se
X-To-Mud-User: povin i Svenskmud.
To: aronsson@lysator.liu.se
X-To-Mud-User: aronsson i Svenskmud.
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Byggande

*** EOOH ***
Date: Sun, 21 Mar 1993 14:10:02 +0100
To: d92maran@und.ida.liu.se
X-To-Mud-User: povin i Svenskmud.
To: aronsson@lysator.liu.se
X-To-Mud-User: aronsson i Svenskmud.
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Byggande

Brev från Gunnar i SvenskMUD.
Kopia till:   aronsson,linus,st,radagast

Hej, gratulerar till magikerskapet.

Hur fasta planer har du f|r ditt bygge? Om de inte {r alltf|r klara har
jag ett f|rslag att ge.

Som du kanske har sett i Lyskom har det uppst}tt en byggkrock i |ster.
Ett viktigt sk{l till det {r Aronsson har varit en smula (n}ja) inaktiv
i sitt byggande vilket har avstannat l}ngt tidigare {n planerna.

I vilket fall {r }tminstone jag ganska angel{gen om att de planerna
f|rverkligas eftersom de bygger p} ett f|rs|k att g|ra Svenskmuds
geografi konsistent (en } fr}n sj|n till havet) och mer svensk
(}kerlandskap och rullstens}s).

Mer information finns p} anslagstavlorna (adv_guild, nr 9,10,
adv_inre_rum, nr 20,21) och i filerna PLAN i /rum/|ster}ker,
/rum/v{stersl{tt och /rum/fr|tuna samt i en del inl{gg i Lyskom som du
antagligen ocks} har l{st.

Mitt f|rslag {r s}ledes att du f|rverkligar en del av de planerna.
Du f}r i stort sett full frihet att utforma omr}det som du sj{lv vill,
kravet {r att geografin v{sentligen st{mmer med planerna.

Om du {r intresserad kan du ta kontakt med mig, och prata g{rna lite
med Aronsson (s} kanske han }terigen blir sugen p} att bygga sj{lv ocks}).

/Gunnar

Kopia av brevet skickas till Aronsson, Linus och {rkemagiker.


1,, svmud,
Summary-line: 24-Mar        Dent@lysator.liu.se  24 #tv} saker...
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA21052; Wed, 24 Mar 1993 00:10:02 +0100
Date: Wed, 24 Mar 1993 00:10:02 +0100
Message-Id: <199303232310.AA21052@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: tv} saker...

*** EOOH ***
Date: Wed, 24 Mar 1993 00:10:02 +0100
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: tv} saker...

Brev från Dent i SvenskMUD.

Haj!

1: hur attan laddar jag mitt castle.c ?

2: Vad kan det vara som g|r att vampyrer f|rlorar sin heart_beat
   efter ett tag... (klona /spelare/dent/vampires/vampire_soul.c,
   jag har kommit ganska l}ngt p} vampyrerna, {ven om jag inte
   {r s{ker p} om jag ska implementera vampyrer i svenskMUD (
    Det kanske inte passar, vad tycker du ? )

mvh Dent



1,,
Summary-line: 25-Mar         martin.zurn@jrc.it  13 #
Received: from relay1.iunet.it (192.106.1.3) by lysator.liu.se (ALPHA-6.36/6.16) id AA16999; Thu, 25 Mar 1993 17:01:12 +0100
Received: from jrc.it (relay.jrc.it) by relay1.iunet.it with SMTP id AA11532
  (5.65c8/IDA-1.4.4 for <linus@lysator.liu.se>); Thu, 25 Mar 1993 17:01:15 +0100
Received: from dac.ise.jrc (dacls1) by jrc.it; Thu, 25 Mar 93 16:59:44 +0100
Received: by dac.ise.jrc (4.1/JRC-S-1.3)
	id AA05128; Thu, 25 Mar 93 17:01:38 +0100
Date: Thu, 25 Mar 93 17:01:38 +0100
From: martin.zurn@jrc.it (Martin ZURN)
Message-Id: <9303251601.AA05128@dac.ise.jrc>
Apparently-To: linus@lysator.liu.se

*** EOOH ***
Date: Thu, 25 Mar 93 17:01:38 +0100
From: martin.zurn@jrc.it (Martin ZURN)
Apparently-To: linus@lysator.liu.se

Hej igen,
	har funnit goda infos oever hamming i Finland. Om Du vill info
maaste Du skriva till listserv@lut.fi teksten av msg:
HEL
LIS
	Finns det lika saker i Sverige?

Ha det saa bra, lycka till


1,,
Summary-line: 26-Mar  : mcguirk@enws302.eas.asu  81 #IRC client for Emacs?
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA29048; Fri, 26 Mar 1993 11:38:22 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA09996; Fri, 26 Mar 1993 11:38:00 +0100
          (unknown)
Date: Fri, 26 Mar 1993 11:38:00 +0100
From: linus@lysator.liu.se
Message-Id: <199303261038.AA09996@bodil.lysator.liu.se>
To: mcguirk@enws302.eas.asu.edu (Dan McGuirk)
In-Reply-To: mcguirk@enws302.eas.asu.edu's message of Fri, 26 Mar 1993 02:04:50 GMT
Subject: IRC client for Emacs?
References: <MCGUIRK.93Mar25190450@enws302.eas.asu.edu>

*** EOOH ***
Date: Fri, 26 Mar 1993 11:38:00 +0100
From: linus@lysator.liu.se
To: mcguirk@enws302.eas.asu.edu (Dan McGuirk)
In-Reply-To: mcguirk@enws302.eas.asu.edu's message of Fri, 26 Mar 1993 02:04:50 GMT
Subject: IRC client for Emacs?
References: <MCGUIRK.93Mar25190450@enws302.eas.asu.edu>

   Does anyone know if there's an irc client for emacs that works
   properly?  I grabbed something called irc.el from the elisp-archive on

The problem is probably the change of protocol for Kiwi (early 1992 i
think).

Sojge, Klaus Zeuge, has written something called Kiwi.el.
Available via ftp from ftp.lysator.liu.se
/ftp.lysator.liu.se:/pub/irc/Kiwi/Kiwi.4.27.el{,c}.Z

Here is the readme file:
The latest version of Kiwi.el which formely was called sojge-irc.el,
is now obtainable by anonymous ftp from ftp.lysator.liu.se
[130236.254.1]. You'll find it in the file pub/emacs/Kiwi-x.y.el where
x.y is the latest version; after getting it, you probaly want to
rename it into Kiwi.el.

This file can either be loaded directly into Emacs by doing

	M-X load-file RET Kiwi.el RET

and then used by doing

	M-X irc RET


A better way may be to first byte-compile the file by

	M-X byte-compile RET Kiwi.el RET

to get the file Kiwi.elc, then to put it in one of the directories
listed in the Emacs variable load-path (you can check the variables
contest by saying

	C-h v load-path RET

) and by putting the line

	(autoload 'irc "Kiwi" 
	    "Internet Relay Chat client." t nil)

in your .emacs file. After this, whenever you say

	M-X irc RET

the irc client is loaded (if it wasn't loaded already) and you are
asked for a hostname where an IRC server can be found.

You can read more about how to intall and how to initialize it for
startup by doing

	/HELP startup

inside Kiwi.


Please let the file be readable by everyone (chmod +r Kiwi*)
on your system.


If you have any problems, or suggestions, feel free to email me at
address sojge@mizar.docs.uu.se or sending me a message on irc. I use
the nickname sojge.

See you on IRC!

	/sojge
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 26-Mar   to: lenst@lysator.liu.se  43 #Virtuella nyhetsgrupper i gnus, fvrsta testen.
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA01351; Fri, 26 Mar 1993 12:22:45 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA10557; Fri, 26 Mar 1993 12:22:19 +0100
          (unknown)
Date: Fri, 26 Mar 1993 12:22:19 +0100
From: linus@lysator.liu.se
Message-Id: <199303261122.AA10557@bodil.lysator.liu.se>
To: lenst@lysator.liu.se
Cc: aronsson@lysator.liu.se
Subject: Virtuella nyhetsgrupper i gnus, fvrsta testen.

*** EOOH ***
Date: Fri, 26 Mar 1993 12:22:19 +0100
From: linus@lysator.liu.se
To: lenst@lysator.liu.se
Cc: aronsson@lysator.liu.se
Subject: Virtuella nyhetsgrupper i gnus, fvrsta testen.

Jag har nu fett de "virtuella nyhetsgrupperna" att fungera hjdlpligt.
Du kan prova om du vill.

M-x load-file ~linus/.el/gnus-virtual

De definierar ett enda nytt kommando:
M-x gnus-Group-read-matching-groups som tar en regexp som argument och
vppnar en grupp med innehellet fren alla de matchande grupperna.

Man kan sedan ldsa, sortera om mm.

"Namnet" pe news-gruppen visas pe mode-raden som
"Virtual: regexp (antal matchar)"

Fvljande fungerar inte:
- att ange en regexp som precis matchar ett gruppnamn pe en
  existerande grupp. De vdljer han den existerande gruppen istdllet.
- gnus-Subject-edit-local-kill (men jag tror att den kvr killfiler som
  heter regexp.KILL och ligger pe rdtt stdlle, det kan du testa).
- Om samma inldgg finns i flera av de matchande grupperna dyker det
  upp flera genger i bufferten.

Fvljande kanske man skulle vilja ha:
- Ett sdtt att se vilka alternativ som finns och hur menga oldsta, som
  jag tdnkte fvrst.
- Ett sdtt att se vilka inldgg som finns i samma newsgroup och vilka
  som finns i andra.
- En speciell hook som bara kvrs om det dr en virtuell grupp (man
  kanske vill ha sort by date i det fallet men inte annars...)
- Ett sdtt att fe reda pe 
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 26-Mar  to: jra@law7.DaytonOH.NCR  91 #Re: Building a newsreader, or reinventing the wheel?
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA04065; Fri, 26 Mar 1993 13:21:37 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA11025; Fri, 26 Mar 1993 13:21:14 +0100
          (unknown)
Date: Fri, 26 Mar 1993 13:21:14 +0100
From: linus@lysator.liu.se
Message-Id: <199303261221.AA11025@bodil.lysator.liu.se>
In-Reply-To: jra@law7.DaytonOH.NCR.COM's message of Wed, 17 Mar 1993 15:13:14 GMT
Newsgroups: news.software.readers
To: jra@law7.DaytonOH.NCR.COM (John Ackermann x 2966)
Subject: Re: Building a newsreader, or reinventing the wheel?
References: <C41Gy3.J8n@law7.DaytonOH.NCR.COM>
Distribution: world

*** EOOH ***
Date: Fri, 26 Mar 1993 13:21:14 +0100
From: linus@lysator.liu.se
In-Reply-To: jra@law7.DaytonOH.NCR.COM's message of Wed, 17 Mar 1993 15:13:14 GMT
Newsgroups: news.software.readers
To: jra@law7.DaytonOH.NCR.COM (John Ackermann x 2966)
Subject: Re: Building a newsreader, or reinventing the wheel?
References: <C41Gy3.J8n@law7.DaytonOH.NCR.COM>
Distribution: world

In article <C41Gy3.J8n@law7.DaytonOH.NCR.COM>
jra@law7.DaytonOH.NCR.COM (John Ackermann x 2966) writes:
   Newsgroups: news.software.readers
   From: jra@law7.DaytonOH.NCR.COM (John Ackermann x 2966)

   Here's the deal:  I run a unix system that ham radio operators can
   access via the radio, running the TCP/IP protocol stack on top of the
   AX.25 protocol.  They can use telnet to log into my system.

   Here's the problem:  the radio channels are very slow, and are half
   duplex to boot.  Most users operate at 1200 baud, and the effective
   throughput is something like 20 or 30 characters per second.  A few
   brave souls have radios that operate at 19.2KB, but even there the
   throughput is only about 500-600 cps, half duplex.
Real optimistics! :-) I have gotten used to the 9600+ environment.

   nn would be a good choice as far as bandwidth goes, but it requires

nn really is short for no news and it is a good choise only if you
read only a few of the articles in every group. If you usually read
all or almost all newsgroups at low baudrates there are competitive
alternatives.

   uncooked keyboard input.

Well, I don't know of a newsreader that really can do this but rn (or
trn) has a feature that might work.

rn is a very much older reader than nn and has been used for quite a
few years. trn is a version that threads the news, not really like nn
that just retrieves the subject lines (and some other infor) to be
able to show them quickly but sorts depending on the contents of the
references field so that you can get trees of comments. You then read
in a depth-first-order and not the nn time of arrival order.

The feature I am thinking about is turned off with -T. This probably
makes it possible to type all commands as command RETURN.

>From the trn manual:

     -T   allows you to type ahead of rn.  Ordinarily rn will eat
          typeahead  to prevent your autorepeating space bar from
          doing a very frustrating thing  when  you  accidentally
          hold it down.  If you don't have a repeating space bar,
          or you are working at low baud rate, you can  set  this
          switch  to  prevent this behavior.  You may wish to use
          the baud-rate switch modifier below to  disable  typea-
          head only at lower baud rates.

This works in this way: 
Most commands are one character commands. When we recieve a command
the commands is (optionally echoed -v option) and then the command is
executed, the screen is updated and the prompt is written.
Then the input buffer is emptied (in your case this could read the
extra RETURN/LINEFEED whatever.)
And then we hang waiting for the next command.

   That still means, though, that I've got to figure out how to process the
   .newsrc file and create the info to be displayed with ipick, and then
   pipe the selected articles into a pager for display.

Wouldn't it be much easier to rewrite nn or trn to fill your
requirements.

   Before I begin this process, a) can anyone point me to a newsreader that
   would meet the uncooked input and low bandwidth requirements, or b) give
   me any pointers on the structure I should use to build one?

There are low baudrate features in the original rn but I don't know
how the news threaded menus are at low baudrate, I think they loose
some, they are optimized for nice presentation and to show as many
articles as possible on every screen.

My advice is: Try it to see if it can do what you want and if you want
to rewrite something to do what you want, rewrite (t)rn, not nn.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Summary-line: 26-Mar  jra@law7.daytonoh.ncr.com 102 #Re: Building a newsreader, or reinventing the wheel?
Received: from ncrcom.DaytonOH.NCR.COM (132.243.130.1) by lysator.liu.se (ALPHA-6.36/6.16) id AA13015; Fri, 26 Mar 1993 16:24:40 +0100
Received: from ciss by ncrcom.DaytonOH.NCR.COM id ab16611; 26 Mar 93 10:18 EST
Received: by ciss.DaytonOH.NCR.COM; 26 Mar 93 09:24:55 EST
Subject: Re: Building a newsreader, or reinventing the wheel?
To: linus@lysator.liu.se
Date: Fri, 26 Mar 93 9:24:43 EST
From: "John R. Ackermann" <jra@law7.daytonoh.ncr.com>
In-Reply-To: <199303261221.AA11025@bodil.lysator.liu.se>; from "linus@lysator.liu.se" at Mar 26, 93 1:21 pm
From: John Ackermann   AG9V <John.Ackermann@DaytonOH.NCR.COM>
Message-Id:  <9303261018.ab16611@ncrcom.DaytonOH.NCR.COM>

*** EOOH ***
Subject: Re: Building a newsreader, or reinventing the wheel?
To: linus@lysator.liu.se
Date: Fri, 26 Mar 93 9:24:43 EST
From: "John R. Ackermann" <jra@law7.daytonoh.ncr.com>
In-Reply-To: <199303261221.AA11025@bodil.lysator.liu.se>; from "linus@lysator.liu.se" at Mar 26, 93 1:21 pm
From: John Ackermann   AG9V <John.Ackermann@DaytonOH.NCR.COM>

Thanks for that information; it's the first useful response I've
received from my posting.  I will look at trn and the -T option;
although RN might work, I would like a program that displays headers
(and several per screen) as the initial mode; most users will not read
the majority of the messages.

John

---------------------------------
You (linus@lysator.liu.se) write:
> 
> In article <C41Gy3.J8n@law7.DaytonOH.NCR.COM>
> jra@law7.DaytonOH.NCR.COM (John Ackermann x 2966) writes:
>    Newsgroups: news.software.readers
>    From: jra@law7.DaytonOH.NCR.COM (John Ackermann x 2966)
> 
>    Here's the deal:  I run a unix system that ham radio operators can
>    access via the radio, running the TCP/IP protocol stack on top of the
>    AX.25 protocol.  They can use telnet to log into my system.
> 
>    Here's the problem:  the radio channels are very slow, and are half
>    duplex to boot.  Most users operate at 1200 baud, and the effective
>    throughput is something like 20 or 30 characters per second.  A few
>    brave souls have radios that operate at 19.2KB, but even there the
>    throughput is only about 500-600 cps, half duplex.
> Real optimistics! :-) I have gotten used to the 9600+ environment.
> 
>    nn would be a good choice as far as bandwidth goes, but it requires
> 
> nn really is short for no news and it is a good choise only if you
> read only a few of the articles in every group. If you usually read
> all or almost all newsgroups at low baudrates there are competitive
> alternatives.
> 
>    uncooked keyboard input.
> 
> Well, I don't know of a newsreader that really can do this but rn (or
> trn) has a feature that might work.
> 
> rn is a very much older reader than nn and has been used for quite a
> few years. trn is a version that threads the news, not really like nn
> that just retrieves the subject lines (and some other infor) to be
> able to show them quickly but sorts depending on the contents of the
> references field so that you can get trees of comments. You then read
> in a depth-first-order and not the nn time of arrival order.
> 
> The feature I am thinking about is turned off with -T. This probably
> makes it possible to type all commands as command RETURN.
> 
> >From the trn manual:
> 
>      -T   allows you to type ahead of rn.  Ordinarily rn will eat
>           typeahead  to prevent your autorepeating space bar from
>           doing a very frustrating thing  when  you  accidentally
>           hold it down.  If you don't have a repeating space bar,
>           or you are working at low baud rate, you can  set  this
>           switch  to  prevent this behavior.  You may wish to use
>           the baud-rate switch modifier below to  disable  typea-
>           head only at lower baud rates.
> 
> This works in this way: 
> Most commands are one character commands. When we recieve a command
> the commands is (optionally echoed -v option) and then the command is
> executed, the screen is updated and the prompt is written.
> Then the input buffer is emptied (in your case this could read the
> extra RETURN/LINEFEED whatever.)
> And then we hang waiting for the next command.
> 
>    That still means, though, that I've got to figure out how to process the
>    .newsrc file and create the info to be displayed with ipick, and then
>    pipe the selected articles into a pager for display.
> 
> Wouldn't it be much easier to rewrite nn or trn to fill your
> requirements.
> 
>    Before I begin this process, a) can anyone point me to a newsreader that
>    would meet the uncooked input and low bandwidth requirements, or b) give
>    me any pointers on the structure I should use to build one?
> 
> There are low baudrate features in the original rn but I don't know
> how the news threaded menus are at low baudrate, I think they loose
> some, they are optimized for nice presentation and to show as many
> articles as possible on every screen.
> 
> My advice is: Try it to see if it can do what you want and if you want
> to rewrite something to do what you want, rewrite (t)rn, not nn.
> -- 
> 	/Linus
> *****	Wherever I exec my `which emacs`, is my $HOME.	*****
> Linus Tolke				SM7OUU, linus@lysator.liu.se
> Student at the				member of SK5EU LiTHSA
> Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)
> 



1,,
Summary-line: 26-Mar  to: jra@law7.daytonoh.ncr  16 #Building a newsreader, or reinventing the wheel?
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA14686; Fri, 26 Mar 1993 17:01:11 +0100
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA12004; Fri, 26 Mar 1993 17:00:44 +0100
          (unknown)
Date: Fri, 26 Mar 1993 17:00:44 +0100
From: linus@lysator.liu.se
Message-Id: <199303261600.AA12004@bodil.lysator.liu.se>
To: jra@law7.daytonoh.ncr.com
In-Reply-To: "John R. Ackermann"'s message of Fri, 26 Mar 93 9:24:43 EST <9303261018.ab16611@ncrcom.DaytonOH.NCR.COM>
Subject: Building a newsreader, or reinventing the wheel?

*** EOOH ***
Date: Fri, 26 Mar 1993 17:00:44 +0100
From: linus@lysator.liu.se
To: jra@law7.daytonoh.ncr.com
In-Reply-To: "John R. Ackermann"'s message of Fri, 26 Mar 93 9:24:43 EST <9303261018.ab16611@ncrcom.DaytonOH.NCR.COM>
Subject: Building a newsreader, or reinventing the wheel?

   received from my posting.  I will look at trn and the -T option;
Please notice that the -T option is used to turn off the
empty-buffer-before-reading-command default behavior.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Summary-line: 28-Mar  ge_Flucht@ltcl.bbs.bad.se  26 #Tja Ba!
Received: from mail.swip.net (192.71.220.11) by lysator.liu.se (ALPHA-6.36/6.16) id AA26511; Sun, 28 Mar 1993 12:09:17 +0200
Received: by mail.swip.net (5.65c8-/1.2)
	id AA02503; Sun, 28 Mar 1993 12:09:15 +0200
Received: by piraya.bad.se (1.65/waf)
	via UUCP; Sun, 28 Mar 93 10:18:16 GMT
	for linus@lysator.liu.se
Received: by f1.n901.z92.badnet.bad.se (0.0.1 BA Datasystem/3C)
	id AA60803; Sun, 28 Mar 93 10:18:15 +0100
Date: Sun, 28 Mar 93 10:16:29 +0100
From: Jan-Inge_Flucht@ltcl.bbs.bad.se (Jan-Inge Flucht)
Subject: Tja Ba!
Message-Id: <OA92-901-203_2bb5c1dc@piraya.bad.se>
Fidonet-Flags: private crash 
To: linus@lysator.liu.se
Reply-To: Jan-Inge_Flucht@ltcl.bbs.bad.se (Jan-Inge Flucht)

*** EOOH ***
Date: Sun, 28 Mar 93 10:16:29 +0100
From: Jan-Inge_Flucht@ltcl.bbs.bad.se (Jan-Inge Flucht)
Subject: Tja Ba!
Fidonet-Flags: private crash 
To: linus@lysator.liu.se
Reply-To: Jan-Inge_Flucht@ltcl.bbs.bad.se (Jan-Inge Flucht)

 
[r Du s{ker p} att fr}gan om "datan{t" via amat|rbanden inte avs}g 
Packet ?? S} tolkade jag det iaf.. 
 
Var finns mer info om projektet p} Lth ?? G}r det att tanka hem 
info via ftp? 
 
Mvh Jinge // 73' 
 
 
  +-+ Author +--------------------------------+ 
  |  Jan-Inge Flucht, Vasterhaninge, SWEDEN   | 
  |  email: internet = jan-inge_flucht@bad.se | 
  |         memonet = kommun.s12.s12jfl       | 
  |         fidonet = 2:201/603               | 
  +-------------------------------------------+ 
 
(SM0LRN)


1, answered,,
Summary-line: 29-Mar  Lennart.Bensryd@LDC.lu.se  14 #H{lsning
Received: from Pollux.lu.se (130.235.132.89) by lysator.liu.se (ALPHA-6.36/6.16) id AA11474; Mon, 29 Mar 1993 10:06:30 +0200
Received: from macpost.lu.se by Pollux.lu.se with SMTP
	(5.61-bind 1.4+ida/IDA-1.2.8) id AA01844; Mon, 29 Mar 93 10:06:43 +0200
Subject: H{lsning
To: Linus@lysator.liu.se
From: Lennart Bensryd <Lennart.Bensryd@LDC.lu.se>
Date: Mon, 29 Mar 93 09:07:17 +0100
Message-Id: <930329.090717.7714@macpost.lu.se>

*** EOOH ***
Subject: H{lsning
To: Linus@lysator.liu.se
From: Lennart Bensryd <Lennart.Bensryd@LDC.lu.se>
Date: Mon, 29 Mar 93 09:07:17 +0100

Hej Linus!
Jag s}g din adress i ett inl{gg i NetNews och snappade upp den.
Hur har du det? Vad syslar du med f|r tillf{llet?

Med v{nlig h{lsning


Lennart Bensryd, tlfn 046 - 107464


1, answered,,
Summary-line: 29-Mar  murray@chemical-eng.edinb  37 #Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: 
Received: from sun2.nsfnet-relay.ac.uk (128.86.8.45) by lysator.liu.se (ALPHA-6.36/6.16) id AA19808; Mon, 29 Mar 1993 14:01:15 +0200
Via: uk.ac.edinburgh.castle; Mon, 29 Mar 1993 12:41:07 +0100
Received: from chemeng.ed.ac.uk by castle.ed.ac.uk id aa03708;
          29 Mar 93 12:40 WET DST
Received: from ugie.chemeng.ed.ac.uk by chemeng.ed.ac.uk;
          Mon, 29 Mar 93 10:26:38 BST
From: murray@chemical-eng.edinburgh.ac.uk
Date: Mon, 29 Mar 93 10:26:25 BST
Message-Id: <18117.9303290926@ugie.chemeng.ed.ac.uk>
To: Linus Tolke Y <linus@lysator.liu.se>
In-Reply-To: linus@lysator.liu.se's message of 26 Mar 93 23:22:16 GMT
Subject: Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: 
         Re: RFD: sci.med.diabetes))

*** EOOH ***
From: murray@chemical-eng.edinburgh.ac.uk
Date: Mon, 29 Mar 93 10:26:25 BST
To: Linus Tolke Y <linus@lysator.liu.se>
In-Reply-To: linus@lysator.liu.se's message of 26 Mar 93 23:22:16 GMT
Subject: Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: 
         Re: RFD: sci.med.diabetes))


You say:

> I have added a possibility to do just this, to retrieve several groups
> at once and to sort the subjects together. (In gnus you can define
> your own sorting order and everything).
> 
> The idea is simple, instead of entering one of the suggested groups
> you type a regexp and you enter what I call a "virtual group" that is
> the union of all matching groups with unread articles.
> 
> Ex. I choose the regexp emacs and get the groups:
> alt.lucid-emacs.bug, alt.lucid-emacs.help, alt.religion.emacs,
> comp.emacs, gnu.emacs.announce, gnu.emacs.bug, gnu.emacs.gnews,
> gnu.emacs.gnus, gnu.emacs.help, gnu.emacs.lisp.manual and
> gnu.emacs.sources.
> 
> (I am unsubscribed to: gnu.emacs.vm.bug, gnu.emacs.vm.info and gnu.emacs.vms)

Could you post or email me a copy of this `hack' I would be most
interested.

Thanks in advance
Murray

 #8*)  #8*)  #8*)  #8*)  _/_/_/   _/     _/ _/   #8*)  #8*)  #8*)  #8*)
D Murray Laing          _/    _/ _/_/ _/_/ _/   Tel: +44 (031) 650 4866
Dept. of Chem. Engng.  _/    _/ _/  _/ _/ _/    EM: murray@uk.ac.ed.chemeng
Univ. of Edinburgh    _/_/_/   _/     _/ _/_/_/ or D.M.Laing@uk.ac.edinburgh


1, answered,,
Summary-line: 29-Mar      mwarren@us.oracle.com  13 #Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: Re: RFD: sci.med.diabetes))
Received: from gatekeeper.oracle.com (139.185.1.5) by lysator.liu.se (ALPHA-6.36/6.16) id AA09896; Mon, 29 Mar 1993 23:20:33 +0200
Received:  from ap207sun.us.oracle.com by gatekeeper.oracle.com (5.59.11/37.7)
	id AA17889; Mon, 29 Mar 93 13:20:29 PST
Received:  by ap207sun.us.oracle.com (5.59.10/37.3)
	id AA01108; Mon, 29 Mar 93 13:20:27 PST
Message-Id: <9303292120.AA01108@ap207sun.us.oracle.com>
Date: Mon, 29 Mar 93 13:20:27 PST
From: Mark Warren <mwarren@us.oracle.com>
To: linus@lysator.liu.se (Linus Tolke Y)
In-Reply-To: linus@lysator.liu.se's message of Fri, 26 Mar 1993 23:22:16 GM
Subject: Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: Re: RFD: sci.med.diabetes))

*** EOOH ***
Date: Mon, 29 Mar 93 13:20:27 PST
From: Mark Warren <mwarren@us.oracle.com>
To: linus@lysator.liu.se (Linus Tolke Y)
In-Reply-To: linus@lysator.liu.se's message of Fri, 26 Mar 1993 23:22:16 GM
Subject: Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: Re: RFD: sci.med.diabetes))


Could you post your small hack?

Thanks,

Mark


1,,
Summary-line: 30-Mar  ge_Flucht@ltcl.bbs.bad.se  27 #Tja Ba!
Received: from varg.lysator.liu.se (130.236.254.151) by lysator.liu.se (ALPHA-6.36/6.16) id AA14427; Tue, 30 Mar 1993 15:58:53 +0200
Received: by varg.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA03890; Tue, 30 Mar 93 15:58:47 +0200
          (unknown)
Date: Tue, 30 Mar 93 15:58:47 +0200
From: linus@lysator.liu.se
Message-Id: <9303301358.AA03890@varg.lysator.liu.se>
To: Jan-Inge_Flucht@ltcl.bbs.bad.se
In-Reply-To: Jan-Inge Flucht's message of Sun, 28 Mar 93 10:16:29 +0100 <OA92-901-203_2bb5c1dc@piraya.bad.se>
Subject: Tja Ba!

*** EOOH ***
Date: Tue, 30 Mar 93 15:58:47 +0200
From: linus@lysator.liu.se
To: Jan-Inge_Flucht@ltcl.bbs.bad.se
In-Reply-To: Jan-Inge Flucht's message of Sun, 28 Mar 93 10:16:29 +0100 <OA92-901-203_2bb5c1dc@piraya.bad.se>
Subject: Tja Ba!

Du skrev:
   [r Du s{ker p} att fr}gan om "datan{t" via amat|rbanden inte avs}g 
   Packet ?? S} tolkade jag det iaf.. 
Nej, det {r jag inte. Men } andra sidan definierar jag packet s}som
paketradiotrafik och softnet {r d} ocks} packet.

   Var finns mer info om projektet p} Lth ?? G}r det att tanka hem 
   info via ftp? 
Lth?, har ingen aning. Jag studerar vid Tekniska H|gskolan i
Link|ping: LiTH.

Du kan nog komma |ver Jens Zanders doktorsavhandling. Annars tror jag
Jean-Jaques Moulis p} isy (Institutionen f|r systemteknik) vet mer.
(Jag kollade just hans emailadress: jj@isy.liu.se).
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  1-Apr  Lennart.Bensryd@LDC.lu.se  25 #H{lsning
Received: from rune.lysator.liu.se (130.236.254.23) by lysator.liu.se (ALPHA-6.36/6.16) id AA23609; Thu, 1 Apr 1993 11:32:12 +0200
Received: by rune.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA06641; Thu, 1 Apr 93 11:32:13 +0200
          (unknown)
Date: Thu, 1 Apr 93 11:32:13 +0200
From: linus@lysator.liu.se
Message-Id: <9304010932.AA06641@rune.lysator.liu.se>
To: Lennart.Bensryd@LDC.lu.se
In-Reply-To: Lennart Bensryd's message of Mon, 29 Mar 93 09:07:17 +0100 <930329.090717.7714@macpost.lu.se>
Subject: H{lsning

*** EOOH ***
Date: Thu, 1 Apr 93 11:32:13 +0200
From: linus@lysator.liu.se
To: Lennart.Bensryd@LDC.lu.se
In-Reply-To: Lennart Bensryd's message of Mon, 29 Mar 93 09:07:17 +0100 <930329.090717.7714@macpost.lu.se>
Subject: H{lsning

   From: Lennart Bensryd <Lennart.Bensryd@LDC.lu.se>
   Date: Mon, 29 Mar 93 09:07:17 +0100

   Hej Linus!
   Jag s}g din adress i ett inl{gg i NetNews och snappade upp den.
Ibland kan det vara bra att sticka ut hakan.
   Hur har du det? Vad syslar du med f|r tillf{llet?

Jag har det bra, trivs med livet, men jag g|r inte s} mycket. Jag
leker mest med datorerna h{r. Jag har blivit beroende tror jag.

Det var kul att h|ra ifr}n dig. Vi ses!
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, edited,,
Summary-line:  1-Apr  o: murray@chemical-eng.ed  19 #Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: 
Received: from rune.lysator.liu.se (130.236.254.23) by lysator.liu.se (ALPHA-6.36/6.16) id AA24062; Thu, 1 Apr 1993 11:45:05 +0200
Received: by rune.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA06651; Thu, 1 Apr 93 11:45:06 +0200
          (unknown)
Date: Thu, 1 Apr 93 11:45:06 +0200
From: linus@lysator.liu.se
Message-Id: <9304010945.AA06651@rune.lysator.liu.se>
To: murray@chemical-eng.edinburgh.ac.uk
In-Reply-To: murray@chemical-eng.edinburgh.ac.uk's message of Mon, 29 Mar 93 10:26:25 BST <18117.9303290926@ugie.chemeng.ed.ac.uk>
To: mwarren@us.oracle.com
In-Reply-To: Mark Warren's message of Mon, 29 Mar 93 13:20:27 PST <9303292120.AA01108@ap207sun.us.oracle.com>
Subject: Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: 
         Re: RFD: sci.med.diabetes))

*** EOOH ***
Date: Thu, 1 Apr 93 11:45:06 +0200
From: linus@lysator.liu.se
To: murray@chemical-eng.edinburgh.ac.uk
In-Reply-To: murray@chemical-eng.edinburgh.ac.uk's message of Mon, 29 Mar 93 10:26:25 BST <18117.9303290926@ugie.chemeng.ed.ac.uk>
To: mwarren@us.oracle.com
In-Reply-To: Mark Warren's message of Mon, 29 Mar 93 13:20:27 PST <9303292120.AA01108@ap207sun.us.oracle.com>
Subject: Virtual newsgroups in gnus (was: Re: Combined Newsgroup Display (was: 
         Re: RFD: sci.med.diabetes))

Well, my small hack is not really ready for wider use but I mail it to
you so you can test and improve.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)

~/.el/gnus-virtual.el included

1,,
Summary-line:  2-Apr  : bug-gnu-emacs@lucid.com  49 #Problems with the screen_title_format and screen_icon_title_format
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA17473; Fri, 2 Apr 1993 12:34:39 +0200
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA03934; Fri, 2 Apr 1993 12:34:36 +0200
          (unknown)
Date: Fri, 2 Apr 1993 12:34:36 +0200
From: linus@lysator.liu.se
Message-Id: <199304021034.AA03934@bodil.lysator.liu.se>
To: bug-gnu-emacs@lucid.com
Subject: Problems with the screen_title_format and screen_icon_title_format

*** EOOH ***
Date: Fri, 2 Apr 1993 12:34:36 +0200
From: linus@lysator.liu.se
To: bug-gnu-emacs@lucid.com
Subject: Problems with the screen_title_format and screen_icon_title_format

In GNU Emacs 19.4 Lucid of Wed Jan 20 1993 on thalidomide (berkeley-unix)

When setting the screen-title-format and screen-icon-title-format to
an expression containing a variable expression the value of the
variables expression is evaluated in the current-buffer while the
screen-title-format and screen-icon-title-format is fetched in the
buffer of the window.

When setting screen-title-format to ("%S: %b" mode-line-conf-name)
with mode-line-conf-name being a buffer-local variable that
information dissapears when I enter another window. It is also
disturbing not to be able to set the icon-title-format to a
buffer-local variable.

The fix is simple:
*** xdisp.c~	Fri Jan  8 03:44:59 1993
--- xdisp.c	Fri Apr  2 10:47:02 1993
***************
*** 3541,3547 ****
    Fset_buffer (w->buffer);
    title_format = Fsymbol_value (Qscreen_title_format);
    icon_format = Fsymbol_value (Qscreen_icon_title_format);
-   Fset_buffer (obuf);
    screen_title_buffer_index = 0;
    display_mode_element (screen_title_display_string, w, 0, 0, 0, 0,
  			sizeof (screen_title_buffer) -1,
--- 3541,3546 ----
***************
*** 3557,3562 ****
--- 3556,3562 ----
  			    icon_format);
      }
    x_set_icon_name_from_char (s, screen_title_buffer);
+   Fset_buffer (obuf);
  }
  
  #endif /* HAVE_X_WINDOWS */
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  5-Apr  : bug-gcc@prep.ai.mit.edu  91 #GCC 2.3.3 - Problem with optimizer on a sequent balance ns32k
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA15228; Mon, 5 Apr 1993 17:13:07 +0200 
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA02651; Mon, 5 Apr 1993 17:13:03 +0200
          (unknown)
Date: Mon, 5 Apr 1993 17:13:03 +0200
From: linus@lysator.liu.se
Message-Id: <199304051513.AA02651@bodil.lysator.liu.se>
To: bug-gcc@prep.ai.mit.edu
Subject: GCC 2.3.3 - Problem with optimizer on a sequent balance ns32k

*** EOOH ***
Date: Mon, 5 Apr 1993 17:13:03 +0200
From: linus@lysator.liu.se
To: bug-gcc@prep.ai.mit.edu
Subject: GCC 2.3.3 - Problem with optimizer on a sequent balance ns32k

I am using:
GCC 2.3.3 configured with balance-dynix
no modifications to the compiler source.
The compiler was compiled with the sequent cc but fold-const.c
regclass.c reload.c jump.c cse.c combine.c was compiled with gcc-2.3.3
(for Dynix 2.0) during stage1. stage2 and stage3 no problems and no
differences between them.

Sequent balance 8000 with ns32k processors running Dynix 3.0.14
Dynix default assembler.

While trying to compile emacs-gayle 18.59 I had some problem with one
file and tracked it down to a problem with the optimizer.

Compiling with -g and -O this code:
================================================================
#define SPACEGLYF ((1 << 8) + ' ') /* Glyf for a space. */
typedef unsigned char char_t;
typedef short unsigned glyf_t;
/* Glyf corresponding to char c: */
static glyf_t
englyf (c)
     char_t c;
{
  return ((c - ' ') * 2 + SPACEGLYF);
}
================================================================

Made the assembler go:
================
Assembler:
"<stdin>", line 31: oper 1, immediate too large
================

And the output of gcc was:
================================================================
    #NO_APP
    gcc2_compiled.:
    .stabs "/usr/gnu/src/emacs-18.59-gayle/src/",100,0,0,Ltext0
    .stabs "testa.c",100,0,0,Ltext0
    .text
    Ltext0:
    .stabs "int:t1=r1;-2147483648;2147483647;",128,0,0,0
    .stabs "char:t2=r2;0;127;",128,0,0,0
    .stabs "long int:t3=r1;-2147483648;2147483647;",128,0,0,0
    .stabs "unsigned int:t4=r1;0;-1;",128,0,0,0
    .stabs "long unsigned int:t5=r1;0;-1;",128,0,0,0
    .stabs "short int:t6=r1;-32768;32767;",128,0,0,0
    .stabs "long long int:t7=r1;0;-1;",128,0,0,0
    .stabs "short unsigned int:t8=r1;0;65535;",128,0,0,0
    .stabs "long long unsigned int:t9=r1;0;-1;",128,0,0,0
    .stabs "signed char:t10=r1;-128;127;",128,0,0,0
    .stabs "unsigned char:t11=r1;0;255;",128,0,0,0
    .stabs "float:t12=r1;4;0;",128,0,0,0
    .stabs "double:t13=r1;8;0;",128,0,0,0
    .stabs "long double:t14=r1;8;0;",128,0,0,0
    .stabs "void:t15=15",128,0,0,0
    .stabs "char_t:t11",128,0,0,0
    .stabs "glyf_t:t8",128,0,0,0
	    .align 1
    _englyf:
	    .stabd 68,0,9
	    enter [],0
	    .stabd 68,0,10
	    movzbd 8(fp),r0
	    lshd 1,r0
>>	    addw 65760,r0
	    movzwd r0,r0
	    .stabd 68,0,11
	    exit []
	    ret 0
    .stabs "englyf:f8",36,0,0,_englyf
    .stabs "c:p1",160,0,0,8
================================================================
addw is add word where word is 16 bits wide.
65760 clearly is bigger than 65535 and I fully understand the
assembler.

While compiling without the -O flag there seems to be no such problem.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Summary-line:  5-Apr    d92perha@und.ida.liu.se  15 #Turist-guide
Received: from ida.liu.se (130.236.139.139; curofix.ida.liu.se) by lysator.liu.se (ALPHA-6.36/6.16) id AA27828; Mon, 5 Apr 1993 21:59:28 +0200 
Received: from und.ida.liu.se (astmatix.ida.liu.se) by ida.liu.se (5.65b/ida.minimaster-V1.0b6d5)
	id AA16955; Mon, 5 Apr 93 21:59:26 +0200
Received: from idefix (idefix-gw) by und.ida.liu.se (5.65b/ida.minimaster-V1.0b6d2)
	id AA07409; Mon, 5 Apr 93 21:59:24 +0200
From: "Per-Anders 'PA' Haraldson" <d92perha@und.ida.liu.se>
Received: from ide19 by idefix (4.1/ida.slave-V1.0b3)
	id AA12189; Mon, 5 Apr 93 21:59:23 +0200
Received: by ide19 (4.1/ida.slave-V1.0b3)
	id AA09658; Mon, 5 Apr 93 21:59:20 +0200
Date: Mon, 5 Apr 93 21:59:20 +0200
Message-Id: <9304051959.AA09658@ide19>
To: y85linto@und.ida.liu.se
Subject: Turist-guide

*** EOOH ***
From: "Per-Anders 'PA' Haraldson" <d92perha@und.ida.liu.se>
Date: Mon, 5 Apr 93 21:59:20 +0200
To: y85linto@und.ida.liu.se
Subject: Turist-guide

       YO !

  Vi är klara med specen och programmet! Tyvärr har vi inte lämnat in specen än. Hur skall documentationen skrivas? När skall allt vara klart?



d92perha      d92mikha

  


1,,
Summary-line:  6-Apr    y85linto@und.ida.liu.se  16 #Re:  Turist-guide
Received: from ida.liu.se (130.236.139.139; curofix.ida.liu.se) by lysator.liu.se (ALPHA-6.36/6.16) id AA02475; Tue, 6 Apr 1993 14:36:37 +0200 
Received: from und.ida.liu.se (astmatix.ida.liu.se) by ida.liu.se (5.65b/ida.minimaster-V1.0b6d5)
	id AA28611; Tue, 6 Apr 93 14:36:35 +0200
Received: by und.ida.liu.se (5.65b/ida.minimaster-V1.0b6d2)
	id AA02529; Tue, 6 Apr 93 14:36:31 +0200
Date: Tue, 6 Apr 93 14:36:31 +0200
From: Linus Tolke <y85linto@und.ida.liu.se>
Message-Id: <9304061236.AA02529@und.ida.liu.se>
To: d92perha@und.ida.liu.se
Subject: Re:  Turist-guide

*** EOOH ***
Date: Tue, 6 Apr 93 14:36:31 +0200
From: Linus Tolke <y85linto@und.ida.liu.se>
To: d92perha@und.ida.liu.se
Subject: Re:  Turist-guide

Jo, allt skall vara klart denna veckan.

G|r s} h{r. L{mna in specen i mitt fack idag.
Jag kommer att vara h{r imorgon och d} kan jag titta p} ert program,
dvs vad det kan g|ra och hur.

D} kan vi ocks} best{mma vad som beh|ver fixas och talas vid om hur
dokumentationen skall se ut.

Vi ses.


1,, svmud,
Summary-line:  6-Apr      Gunnar@lysator.liu.se  29 #Vampyrgille
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA21843; Tue, 6 Apr 1993 23:10:08 +0200 
Date: Tue, 6 Apr 1993 23:10:08 +0200
Message-Id: <199304062110.AA21843@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Vampyrgille

*** EOOH ***
Date: Tue, 6 Apr 1993 23:10:08 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Vampyrgille

Brev från Gunnar i SvenskMUD.
Kopia till:   st,radagast

Dent har per brev bett mig att titta p} ett vampyrgille som han har gjort.
Sj{lv har jag inte tid just nu men jag har t{nkt att g|ra det i p}sk.
Aktuell fil {r /spelare/dent/vampires/vampire.soul (ungef{r).

En snabb titt i filen visade att den inneh}ller en funktion som ger
numerisk information om egenskaper bland annat. Det verkar ol{mpligt
tycker jag.

Vidare {r jag inte s{ker p} att jag alls gillar vampyriden men jag
kanske {r lite k{nslig...

/Gunnar


1,,
Summary-line:  7-Apr         rms@gnu.ai.mit.edu  24 #Re: GCC 2.3.3 - Problem with optimizer on a sequent balance ns32k
Received: from mole.gnu.ai.mit.edu (128.52.46.33) by lysator.liu.se (ALPHA-6.36/6.16) id AA12009; Wed, 7 Apr 1993 08:43:53 +0200 
Received: by mole.gnu.ai.mit.edu (5.65/4.0)
	id <AA10740@mole.gnu.ai.mit.edu>; Wed, 7 Apr 93 02:43:47 -0400
Date: Wed, 7 Apr 93 02:43:47 -0400
From: rms@gnu.ai.mit.edu (Richard Stallman)
Message-Id: <9304070643.AA10740@mole.gnu.ai.mit.edu>
To: linus@lysator.liu.se
In-Reply-To: <199304051513.AA02651@bodil.lysator.liu.se> (linus@lysator.liu.se)
Subject: Re: GCC 2.3.3 - Problem with optimizer on a sequent balance ns32k

*** EOOH ***
Date: Wed, 7 Apr 93 02:43:47 -0400
From: rms@gnu.ai.mit.edu (Richard Stallman)
To: linus@lysator.liu.se
In-Reply-To: <199304051513.AA02651@bodil.lysator.liu.se> (linus@lysator.liu.se)
Subject: Re: GCC 2.3.3 - Problem with optimizer on a sequent balance ns32k

Thanks.  The current version produces different output,
so maybe the bug is gone.  (If not, I'll have to wait till
someone reproduces it some other way.)

	.file	"tst6.c"
gcc2_compiled.:
___gnu_compiled_c:
.text
	.align 16
_englyf:
	enter [],0
	movzbd 8(fp),r0
	addw r0,r0
	addw $224,r0
	movzwd r0,r0
	exit []
	ret 0


1, answered,, svmud,
Summary-line:  7-Apr     joachim@fy.chalmers.se  21 #Re: Inbjudan till SvenskMUD-tr{ff, s|ndagen den 28 mars.
Received: from chalmers.se (129.16.1.1) by lysator.liu.se (ALPHA-6.36/6.16) id AA12478; Wed, 7 Apr 1993 08:57:49 +0200 
Received: from fyserv1.fy.chalmers.se by chalmers.se (5.60+IDA/3.14+gl) id AA26017; Wed, 7 Apr 93 08:57:47 +0200
From: Joachim Bergr` <joachim@fy.chalmers.se>
Message-Id: <9304070657.AA04554@fyserv1.fy.chalmers.se>
Received: from lento by fyserv1.fy.chalmers.se (4.1/3.14+gl) id AA04554; Wed, 7 Apr 93 08:57:45 +0200
Received: by lento id AA21258; Wed, 7 Apr 93 08:57:43 +0200
Subject: Re: Inbjudan till SvenskMUD-tr{ff, s|ndagen den 28 mars.
To: linus@lysator.liu.se
Date: Wed, 7 Apr 1993 08:57:42 +0200 (MET DST)
In-Reply-To: <199303032153.AA15020@bodil.lysator.liu.se> from "linus@lysator.liu.se" at Mar 3, 93 10:53:19 pm
X-Mailer: ELM [version 2.4 PL17]
Mime-Version: 1.0
Content-Type: text/plain; charset=US-ASCII
Content-Transfer-Encoding: 7bit
Content-Length: 363       

*** EOOH ***
From: Joachim Bergr` <joachim@fy.chalmers.se>
Subject: Re: Inbjudan till SvenskMUD-tr{ff, s|ndagen den 28 mars.
To: linus@lysator.liu.se
Date: Wed, 7 Apr 1993 08:57:42 +0200 (MET DST)
In-Reply-To: <199303032153.AA15020@bodil.lysator.liu.se> from "linus@lysator.liu.se" at Mar 3, 93 10:53:19 pm
X-Mailer: ELM [version 2.4 PL17]
Mime-Version: 1.0
Content-Transfer-Encoding: 7bit

Hejsan hejsan ...

Erk{nner att jag var lite lat som inte svarade p} detta brev lite tidigare men
har haft lite ont om tid som en d}lig f|rklaring...

Hursomhelst s} skulle jag vara intresserad av vad ni kom fram till under dessa
dagar d} jag till sommaren f|rhoppningsvis har lite mera tid och d} kanske 
skulle kunna bygga ngt...

MVH Joachim Bergre' (Merlin)



1,, svmud,
Summary-line:  7-Apr  o: joachim@fy.chalmers.se  21 #Inbjudan till SvenskMUD-tr{ff, s|ndagen den 28 mars.
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA00784; Wed, 7 Apr 1993 17:34:30 +0200 
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA04603; Wed, 7 Apr 1993 17:34:25 +0200
          (unknown)
Date: Wed, 7 Apr 1993 17:34:25 +0200
From: linus@lysator.liu.se
Message-Id: <199304071534.AA04603@bodil.lysator.liu.se>
To: joachim@fy.chalmers.se
In-Reply-To: Joachim Bergr`'s message of Wed, 7 Apr 1993 08:57:42 +0200 (MET DST) <9304070657.AA04554@fyserv1.fy.chalmers.se>
Subject: Inbjudan till SvenskMUD-tr{ff, s|ndagen den 28 mars.

*** EOOH ***
Date: Wed, 7 Apr 1993 17:34:25 +0200
From: linus@lysator.liu.se
To: joachim@fy.chalmers.se
In-Reply-To: Joachim Bergr`'s message of Wed, 7 Apr 1993 08:57:42 +0200 (MET DST) <9304070657.AA04554@fyserv1.fy.chalmers.se>
Subject: Inbjudan till SvenskMUD-tr{ff, s|ndagen den 28 mars.

Egentligen beslutade vi inte s} mycket utan vi pratade mest.

Den intressantaste diskussionen var vad g{ller v{rldens utseende.
Vi har ganska bra kartor ritade nu f|r hand.

Jag funderar p} att }ka ner till er till Chalmersrocken. Kan vi
tr{ffas d} (alla SvenskMUD-magiker) s} kan vi prata lite kring mudden
vid det tillf{llet? Det vore kul tycker jag.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Summary-line:  7-Apr  : sbarrell@acpub.duke.edu  54 #Registration not required
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA01085; Wed, 7 Apr 1993 17:42:50 +0200 
Received: by bodil.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA04622; Wed, 7 Apr 1993 17:42:46 +0200
          (unknown)
Date: Wed, 7 Apr 1993 17:42:46 +0200
From: linus@lysator.liu.se
Message-Id: <199304071542.AA04622@bodil.lysator.liu.se>
To: sbarrell@acpub.duke.edu
In-Reply-To: Stephen Barrell's message of Wed, 7 Apr 1993 01:22:37 -0400 (EDT) <9304070522.AA19302@raphael.acpub.duke.edu>
Subject: Registration not required

*** EOOH ***
Date: Wed, 7 Apr 1993 17:42:46 +0200
From: linus@lysator.liu.se
To: sbarrell@acpub.duke.edu
In-Reply-To: Stephen Barrell's message of Wed, 7 Apr 1993 01:22:37 -0400 (EDT) <9304070522.AA19302@raphael.acpub.duke.edu>
Subject: Registration not required

   From: sbarrell@acpub.duke.edu (Stephen Barrell)
   Date: Wed, 7 Apr 1993 01:22:37 -0400 (EDT)

   Hello.  I would like to register for NannyMud.  i understand that
   this is the place to do so. 

This is not correct. You can reach and enter nannymud without registering.

	      i read and write swedish, though I do not know if that is
   necessary in this case. 

This is not required if you play nannymud.

			 i think it would be fun to play together with 
   Swedes.  I lived in Stockholm for four years!  Hope to hear from you. 

If you want to play together with Swedes I don't think nannymud is the
place for you, there are mostly (ignorant) americans and greeks there :-)


Now, if I substitute SvenskMUD:
   Hello.  I would like to register for SvenskMUD.  i understand that
   this is the place to do so.

Yes it is. Well you don't really need to register anymore, you could
enter the game and create yourself a character on login.

  		i read and write swedish, though I do not know if that is
   necessary in this case.

Yes it is. All communications in the game are in Swedish.

			  i think it would be fun to play together with 
   Swedes.  I lived in Stockholm for four years!  Hope to hear from you. 

There are mostly Swedish players. Well I have seen a finn sometime and
some englishmen with a huge amount of enthusiasm.

V{lkommen till SvenskMUD!

telnet svmud.lysator.liu.se 2043
-- 
	/Linus, Gud i SvenskMUD
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line:  7-Apr  : sbarrell@acpub.duke.edu  19 #Registration not required
Received: from robin.lysator.liu.se (130.236.254.21) by lysator.liu.se (ALPHA-6.36/6.16) id AA03879; Wed, 7 Apr 1993 18:57:06 +0200 
Received: by robin.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA18636; Wed, 7 Apr 93 18:56:45 +0200
          (unknown)
Date: Wed, 7 Apr 93 18:56:45 +0200
From: linus@lysator.liu.se
Message-Id: <9304071656.AA18636@robin.lysator.liu.se>
To: sbarrell@acpub.duke.edu
In-Reply-To: linus@lysator.liu.se's message of Wed, 7 Apr 1993 17:42:46 +0200 <199304071542.AA04622@bodil.lysator.liu.se>
Subject: Registration not required

*** EOOH ***
Date: Wed, 7 Apr 93 18:56:45 +0200
From: linus@lysator.liu.se
To: sbarrell@acpub.duke.edu
In-Reply-To: linus@lysator.liu.se's message of Wed, 7 Apr 1993 17:42:46 +0200 <199304071542.AA04622@bodil.lysator.liu.se>
Subject: Registration not required

   telnet svmud.lysator.liu.se 2043

If you have a terminal that can display iso-8859-1 and a
telnet-program that doesn't eat them you could try the 2046 port.

   telnet svmud.lysator.liu.se 2046.
-- 
	/Linus, Gud i SvenskMUD
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,, svmud,
Summary-line:  9-Apr      Gunnar@lysator.liu.se  53 #Ang}ende vampyrsj{len
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA29001; Fri, 9 Apr 1993 17:10:01 +0200 
Date: Fri, 9 Apr 1993 17:10:01 +0200
Message-Id: <199304091510.AA29001@lysator.liu.se>
To: dent@krynn.solace.hsh.se
X-To-Mud-User: dent i Svenskmud.
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Ang}ende vampyrsj{len

*** EOOH ***
Date: Fri, 9 Apr 1993 17:10:01 +0200
To: dent@krynn.solace.hsh.se
X-To-Mud-User: dent i Svenskmud.
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Ang}ende vampyrsj{len

Brev från Gunnar i SvenskMUD.
Kopia till:   linus,st,radagast

Jag har testat den nu och tittat p} koden.
N}gra kommentarer:

* Skr{mfunktionen var kul. Kan m|jligen ha ol{mpliga bieffekter som att
  monster blir f|r l{tta att bli av med. Jag kan dock inte se n}got
  omedelbart exempel.

* biofunktionen ger alldeles f|r exakta upplysningar. Det finns sk{l
  till att egenskaperna inte beskrivs med numeriska v{rden n{r man g|r
  po{ng egenskaper. P} samma s{tt kan behandlingen av pengar komma att
  {ndras i framtiden. Det finns redan planer }t det h}llet.

* Att bita andra spelare f|r att sprida smitta kan iofs verka naturligt
  men jag tycker att det {r oacceptabelt att offren inte kan f|rsvara sig,
  s{rskilt som det leder b}de till hp-f|rluster och en massa irriterande
  texter (s{rskilt femtielfte g}ngen...).
  N{r man biter n}gon borde det v{l }tminstone bli strid.

Huruvida ett vampyrgille som s}dant {r en lysande ide eller inte t}l ocks}
att funderas |ver. Jag har lite sv}rt att inse att ett "gille" av
vampyrer, som f|r|kar sig genom att bita andra, tillf|r s{rskilt mycket
till Svenskmud, men jag t{nker inte d{rf|r f|rs|ka hindra att det dyker
upp utan n|ja mig med att sucka och undra |ver varf|r ingen vill g|ra
lite mer traditionella gillen. Var finns t.ex. hantverkargillet,
tjuvgillet, helbr{gdagillet och f|r att inte bara tala om spelmansgillet?

Nog om detta, eventuella buggar f}r du ta hand om sj{lv. Jag t{nker inte
godk{nna bio- och bitfunktionerna som de ser ut nu men i |vrigt verkade
det vid en snabb |verblick vara acceptabelt. Det k{ndes sk|nt att det
inte var nerlusat med en massa v{rstingtrollformler i stil med
vintervargsamuletten.

/Gunnar
(kopia till Linus och {rkemagiker)


1,, svmud,
Summary-line: 13-Apr     Testola@lysator.liu.se  25 #Mitt uppdrag
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA16858; Tue, 13 Apr 1993 13:10:02 +0200 
Date: Tue, 13 Apr 1993 13:10:02 +0200
Message-Id: <199304131110.AA16858@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Testola@lysator.liu.se (Testola i Svenskmud)
Reply-To: ceder@nada.kth.se (Testola i Svenskmud)
X-From-Mud-User: Testola i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Mitt uppdrag

*** EOOH ***
Date: Tue, 13 Apr 1993 13:10:02 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Testola@lysator.liu.se (Testola i Svenskmud)
Reply-To: ceder@nada.kth.se (Testola i Svenskmud)
X-From-Mud-User: Testola i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Mitt uppdrag

Brev från Testola i SvenskMUD.

Ja nu {r jag f{rdig med uppdateringen av mitt uppdrag. Dessutom f|ljde
jag Tuborgs sista sk{lvande steg p} v{gen mot l|sningen av det samma.
Han {r den f|rste som l|st det och det visade sig att jag fick d|pa om
mitt uppdragsobjekt f|r att det skulle fungera (resultat av slarv fr}n
min sida allts}).

Han tyckte det var sv}rt, men hade inte l{st s{rskilt mycket i mitt
bibliotek (han fick skylla sig sj{lv s.a.s :-) ). Av n}gon anledning
hade han missat alverna varf|r han inte riktigt hade klart f|r sig 
vad sj{lva uppdraget bestod i. Men det det gick uppenbarligen att l|sa i alla fall! :)

V{nligen
Testola


1, answered,,
Summary-line: 14-Apr   kpc@ptolemy.arc.nasa.gov  15 #Virtual groups
Received: from ptolemy.arc.nasa.gov (128.102.114.134; ptolemy-ethernet.arc.nasa.gov) by lysator.liu.se (ALPHA-6.36/6.16) id AA10457; Thu, 15 Apr 1993 04:02:52 +0200 
Received: from zog.arc.nasa.gov by ptolemy.arc.nasa.gov (4.1/) id <AA14752>; Wed, 14 Apr 93 19:05:09 PDT
Date: Wed, 14 Apr 93 19:05:08 PDT
Message-Id: <9304150205.AA14752@ptolemy.arc.nasa.gov>
Received: by zog.arc.nasa.gov (4.1/SMI-4.1)
	id AA11159; Wed, 14 Apr 93 19:04:44 PDT
From: Kimball Collins <kpc@ptolemy.arc.nasa.gov>
Sender: kpc@ptolemy.arc.nasa.gov
To: linus@lysator.liu.se (Linus Tolke Y)
Subject: Virtual groups
Newsgroups: gnu.emacs.gnus
In-Reply-To: <LINUS.93Mar27002216@bodil.lysator.liu.se>
Reply-To: kpc@ptolemy.arc.nasa.gov

*** EOOH ***
Date: Wed, 14 Apr 93 19:05:08 PDT
From: Kimball Collins <kpc@ptolemy.arc.nasa.gov>
Sender: kpc@ptolemy.arc.nasa.gov
To: linus@lysator.liu.se (Linus Tolke Y)
Subject: Virtual groups
Newsgroups: gnu.emacs.gnus
In-Reply-To: <LINUS.93Mar27002216@bodil.lysator.liu.se>
Reply-To: kpc@ptolemy.arc.nasa.gov

hi.

have you been using your virtual groups code and still like it?  it
might be a good thing to make part of gnus.  would you be willing to
let me be a beta tester?


1,,
Summary-line: 16-Apr  to: vax@ccwf.cc.utexas.ed  72 #A New Mud Driver System (not LP)
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA29205; Fri, 16 Apr 1993 21:02:40 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA23715; Fri, 16 Apr 1993 21:02:24 +0200 
Date: Fri, 16 Apr 1993 21:02:24 +0200
From: linus@lysator.liu.se
Message-Id: <199304161902.AA23715@bodil.lysator.liu.se>
To: vax@ccwf.cc.utexas.edu (Vax)
In-Reply-To: vax@ccwf.cc.utexas.edu's message of 9 Apr 1993 09:38:21 GMT
Subject: A New Mud Driver System (not LP)

*** EOOH ***
Date: Fri, 16 Apr 1993 21:02:24 +0200
From: linus@lysator.liu.se
To: vax@ccwf.cc.utexas.edu (Vax)
In-Reply-To: vax@ccwf.cc.utexas.edu's message of 9 Apr 1993 09:38:21 GMT
Subject: A New Mud Driver System (not LP)

I think this sounds interesting. I just have a few questions.

You wrote:
			   mutable code (like lisp)
			   dynamic inheritance (unlike anything else!)
What do you mean by dynamic inheritance? Is that some kind of: 
(if (thisistrue) (inherit thisfile) (inherit thatfile))

			   forking and execing (like Unix)
			   copy-on-write paging system
What? forking and execing what? Are all objects separate processes? In
that case, why not let the operating handle (or not handle) the
copy-on-write mechanism like it should?
			   IPC with other programs (and muds)
This is a good idea to have in mind already in the initial planning.
I think that this is one of the things that could make this
interesting enough for people to switch to this idea instead. But most
of the problems is with the administration of the muds. You will have
to create a group of administrators with good communications that runs
their mud. This is hard to organize.

	   tons of neat data types like:
		   {signed,unsigned}{int short long quad}
		   longints (like lisp)
		   float double
		   char
		   string (with perl-like regexp pattern matching)
		   sockets (as a built in data type!)
		   array list associative-array
		   semaphores sharedmemory fifos (still to come)
What? Are you not a little bit LPC-oriented now. In lisps and object
oriented languages the types are more often not treated with this much
attention.

If you want to make it easy for you I suggest you restict it to fewer
types and implement the rest of the wanted types in the lisp-like
language. My suggestions: int and lists. With these you can build all
the other types using you language. If you then find the handling of
the special types to slow (for doing floating point operations or
searches in the associative arrays _then_ you can consider entering
them in the language itself.

   Sound too good to be true?  It is, right now.  I'm working on the interpreter
   core right now.  Byte-compiled lispy smalltalk stuff.  I've never written a
   compiler/interpreter before, so anyone with suggestions of stuff to look at,
Why go to byte-compilation? Make an interpreter and if the result is
too slow, write a function for interpreting byte-compiled expressions.
This makes it possible to have something working faster.

   this programming language/Mud language/Operating System is a programmer's
   dream.  Hopefully the advantages (CPOW, dynamic inheritance) will outweigh
   the disadvantages (a complex programming language)
This I want to see before I believe you. My feeling is that a more
complex programming language is not easier. I would suggest you
instead took a well-known well-documented language (like CLOS or
smalltalk) and use that with small modifiations. Then all wizards (the
people building things) can go to the local bookstore and buy a book
on the language instead of you writing a documentation and then in
some way distribute that.
-- 
	/Linus, Gud i SvenskMUD
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 17-Apr  to: root@lysator.liu.se,   61 #ftpd patchad.
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA18831; Sat, 17 Apr 1993 06:29:42 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA00866; Sat, 17 Apr 1993 06:29:26 +0200 
Date: Sat, 17 Apr 1993 06:29:26 +0200
From: linus@lysator.liu.se
Message-Id: <199304170429.AA00866@bodil.lysator.liu.se>
To: root@lysator.liu.se, 134@lyskom.lysator.liu.se
In-Reply-To: <341768@lyskom.lysator.liu.se>
Subject: ftpd patchad.

*** EOOH ***
Date: Sat, 17 Apr 1993 06:29:26 +0200
From: linus@lysator.liu.se
To: root@lysator.liu.se, 134@lyskom.lysator.liu.se
In-Reply-To: <341768@lyskom.lysator.liu.se>
Subject: ftpd patchad.

Nu har jag patchat ftpd s} att den klarar av att skriva filer som {r
kortare {n den var nyss (utan att l{mna skr{p p} slutet.)

H{r {r {ndringen:
***************
*** 1370,1379 ****
       * else open the file and let the default umask determine the file mode. */
      if (f_mode > 0) {
          oldmask = umask(0000);
!         fdout = open(name, O_RDWR | O_CREAT, f_mode);
          umask(oldmask);
      } else
!         fdout = open(name, O_RDWR | O_CREAT, 0666);
  
      if (fdout < 0) {
          perror_reply(553, name);
--- 1370,1379 ----
       * else open the file and let the default umask determine the file mode. */
      if (f_mode > 0) {
          oldmask = umask(0000);
!         fdout = open(name, O_RDWR | O_CREAT  | O_TRUNC, f_mode);
          umask(oldmask);
      } else
!         fdout = open(name, O_RDWR | O_CREAT | O_TRUNC, 0666);
  
      if (fdout < 0) {
          perror_reply(553, name);

Om ingen i lysator har {ndrat just denna delen av koden kanske det {r
p} sin plats att s{nda en patch till de som underh}ller denna ftpd.

ftpd ligger uppkompilerad (f|r sun4) just nu i
/usr/local/src/sys/wu-ftpd-2.0-L1/src/in.ftpd.

Installera denna (flytta till /usr/wheel/sbin/in.ftpd), kompilera f|r
sun3 och installera, kompilera f|r quetzalcoatl och kompilera...

Under mina f|rs|k att kompilera kom jag till slutsatsen att raden:
setenv C_INCLUDE_PATH "/usr/local/include:/usr/gnu/var/gcc/sunos4.1.1/2.1/include:/usr/include"
i /etc/path.csh

Borde ers{ttas med:
setenv C_INCLUDE_PATH "/usr/local/include:/usr/gnu/lib/gcc-lib/sun4-sunos4.1/2.3.3/include:/usr/include"
P} sun4 och
setenv C_INCLUDE_PATH "/usr/local/include:/usr/gnu/lib/gcc-lib/sun3-sunos4.1/2.3.3/include:/usr/include"
p} sun3 eller till
setenv C_INCLUDE_PATH "/usr/local/include"
p} b}da systemen.
--
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 17-Apr     vax@ccwf.cc.utexas.edu 102 #Re: A New Mud Driver System (not LP)
Received: from sylvester.cc.utexas.edu (128.83.135.63) by lysator.liu.se (ALPHA-6.36/6.16) id AA25710; Sun, 18 Apr 1993 00:19:13 +0200 
Received: by sylvester.cc.utexas.edu (5.61/1.34/CCWF 1.21)
	id AA09704; Sat, 17 Apr 93 17:19:08 -0500
From: vax@ccwf.cc.utexas.edu (Vax)
Message-Id: <9304172219.AA09704@sylvester.cc.utexas.edu>
Subject: Re: A New Mud Driver System (not LP)
To: linus@lysator.liu.se
Date: Sat, 17 Apr 1993 17:19:06 -0500 (CDT)
In-Reply-To: <199304161902.AA23715@bodil.lysator.liu.se> from "linus@lysator.liu.se" at Apr 16, 93 09:02:24 pm
X-Mailer: ELM [version 2.4 PL21]
Content-Type: text
Content-Length: 5096      

*** EOOH ***
From: vax@ccwf.cc.utexas.edu (Vax)
Subject: Re: A New Mud Driver System (not LP)
To: linus@lysator.liu.se
Date: Sat, 17 Apr 1993 17:19:06 -0500 (CDT)
In-Reply-To: <199304161902.AA23715@bodil.lysator.liu.se> from "linus@lysator.liu.se" at Apr 16, 93 09:02:24 pm
X-Mailer: ELM [version 2.4 PL21]

While really really bored, linus@lysator.liu.se wrote:
> 
> I think this sounds interesting. I just have a few questions.
> 
> What do you mean by dynamic inheritance? Is that some kind of: 
> (if (thisistrue) (inherit thisfile) (inherit thatfile))
Well, more or less.  Inheriting can take place at any time, since the code
is not compiled but rather interpreted (well.. it's compiled to small-opcodes
to save space, much like perl)
> What? forking and execing what? Are all objects separate processes? In
Nah.  Objects can make copies of themselves in the same way processes
make copies of themselves; a given object can make a copy with the same
"state" by forking, or it could "exec" another object; changing it's
behaviour while keeping the pointers to itself from other objects.
> 			   IPC with other programs (and muds)
> This is a good idea to have in mind already in the initial planning.
> I think that this is one of the things that could make this
> interesting enough for people to switch to this idea instead. But most
> of the problems is with the administration of the muds. You will have
> to create a group of administrators with good communications that runs
> their mud. This is hard to organize.
Good point.  I plan on having all the admins having an account on the mud
machine, so that they can use the Unix utilities to program, and communicate.
Muds are not a very useful medium of communication (I like talk(1) much
better  :-)
> 
> 	   tons of neat data types like:
> 		   {signed,unsigned}{int short long quad}
> 		   longints (like lisp)
> 		   float double
> 		   char
> 		   string (with perl-like regexp pattern matching)
> 		   sockets (as a built in data type!)
> 		   array list associative-array
> 		   semaphores sharedmemory fifos (still to come)
> What? Are you not a little bit LPC-oriented now. In lisps and object
> oriented languages the types are more often not treated with this much
> attention.
Yes and no.  You have to build in the types as atoms, just like lisp does.
There is a reason why lisp defines string constants and chars, you know.
> 
> If you want to make it easy for you I suggest you restict it to fewer
> types and implement the rest of the wanted types in the lisp-like
> language. My suggestions: int and lists. With these you can build all
> the other types using you language. If you then find the handling of
Not the IPC things.  But I get your ppoint.
> the special types to slow (for doing floating point operations or
> searches in the associative arrays _then_ you can consider entering
> them in the language itself.
That will probably be the development cycle.
> 
> Why go to byte-compilation? Make an interpreter and if the result is
> too slow, write a function for interpreting byte-compiled expressions.
> This makes it possible to have something working faster.
My plan is based on the end product; as before, I will probably have
a non-byte code compiled version first.  But it's so easy, I'm at least
going to plan on doing it.
> 
> This I want to see before I believe you. My feeling is that a more
> complex programming language is not easier. I would suggest you
> instead took a well-known well-documented language (like CLOS or
> smalltalk) and use that with small modifiations. Then all wizards (the
> people building things) can go to the local bookstore and buy a book
> on the language instead of you writing a documentation and then in
> some way distribute that.
I couldn't agree more.  I am looking for an OS that does all I want.
However, my posts on the net don't receive many replies.  Someone told
me to check out MOP running on CLOS.  But as I only know scheme, I am going
to have to study that for a while before I can even evaluate the language!
Smalltalk is fairly developed, but ugly in syntax, lacking in AI code,
and lacking of dynamic inheritance, as far as I know.  If anything is the
basis, it will be an object oriented lisp language.
> -- 
> 	/Linus, Gud i SvenskMUD
> *****	Wherever I exec my `which emacs`, is my $HOME.	*****
> Linus Tolke				SM7OUU, linus@lysator.liu.se
> Student at the				member of SK5EU LiTHSA
> Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)
> 
Thanks for your input, new angles on things keep my concept finely
honed and often they flesh out the details I otherwise wouldn't have
thought about.  I have high hopes for Meta-Object Protocol on top of
CLOS, but it may take me a few months to become familiar with it.
Fortunately, I cna pick up CLOS for 386BSD and play around with it,
look at the src code, etc...
Some people have suggested I start looking at the interpreter cores for
such languages as Smalltalk, CLOS, Tinymu{ck,sh}, Lambdamoo, Coolmud, etc.
Do you know how close some of the muds come to my ideal?  It is hard
to find people who know much about more than one.  Lambdamoo holds great
promise, and coolmud supposedley has several good ideas;
know which I should look at first?
-- 
"God protect me from my friends, I can protect myself from my enemies" Voltaire
VaX#n8 vax@ccwf.cc.utexas.edu vax@wixer.UUCP vax@lndead.UUCP


1,,
Summary-line: 18-Apr     to: bug@lysator.liu.se  65 #Svenskmud-treffen
Received: from lysita.lysator.liu.se (130.236.254.153) by lysator.liu.se (ALPHA-6.36/6.16) id AA24326; Sun, 18 Apr 1993 14:29:44 +0200 
Received: by lysita.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA29318; Sun, 18 Apr 1993 14:28:41 +0200
          (unknown)
Date: Sun, 18 Apr 1993 14:28:41 +0200
Message-Id: <199304181228.AA29318@lysita.lysator.liu.se>
To: bug@lysator.liu.se
Subject: Svenskmud-treffen
From: linus@lysator.liu.se
Reply-To: linus@lysator.liu.se
Cc: linus@lysator.liu.se
Sender: ceder@lysator.liu.se

*** EOOH ***
Date: Sun, 18 Apr 1993 14:28:41 +0200
To: bug@lysator.liu.se
Subject: Svenskmud-treffen
From: linus@lysator.liu.se
Reply-To: linus@lysator.liu.se
Cc: linus@lysator.liu.se
Sender: ceder@lysator.liu.se

SvenskMUD-tr{ffen den 28 mars 1993

Historiens f|rsta SvenskMUD-tr{ff gick av stapeln s|ndagen den 28
mars. M}let med tr{ffen var att l{ra k{nna olika magiker, ansikte mot
ansikte och ocks} samordna verksamheten med v{rldsbyggandet.

N{rvarande var n}gra magiker fr}n SvenskMUD, vissa hade rest till fr}n
andra delar av v}rt l}nga land men de allra flesta kom lokalt fr}n
Link|ping.

Schemat f|r dagen var inte riktigt v{l genomt{nkt, det fanns
allvarliga brister p} flera punkter - mest tids och lokalm{ssigt men
vi hann med vad vi ville och det viktiga {r v{l att man f}r n}got
gjort, inte att planeringen st{mmer.

F|rsta officiella delen handlade om SvenskMUDs historia, se tidigare
nummer av garb, och dessutom lite om hur objekt byggs upp och
samarbetar. Just detta samarbete {r den sv}raste delen i hela
programmeringsprocessen. Det finns n{mligen ingen bra dokumentation om
hur det g|rs och vad som egentligen h{nder. Det enda som finns {r
/doc/lfun/*. Dessutom {r koden som sk|ter de grundl{ggande objekten
inte riktigt bra, den {r gammal, ursprungligen skriven f|r en helt
annan driver.

Under punkten erfarenhetsutbyte om kodning blev det inte s} mycket
prat. Det verkar som om det inte {r vidare l{mpligt att diskutera
s}dana fr}gest{llningar - det {r alldeles f|r sm} detaljer f|r att det
skall bli diskussion av det hela.

Nu var det dags f|r det obligatoriska Snoddaskorvenbes|ket. Oturligt
nog hade n}gon magiker i verkliga livet klonat fram en massa sn| |ver
oss precis n{r vi hade kommit dit. Det blev lite kyligt men h{rdade
magiker st}r ut.

Den intressantaste delen av diskussionen var den som handlade om hur
v{rlden ser ut nu och hur den skall se ut i forts{ttningen. F|r
n{rvarande {r v{rlden konsistent d} tillvida att man ganska l{tt kan
rita en karta och pricka in omr}den s}som skog, sj|, hed, berg mm och
det st{mmer n}gots}n{r. Med lite vilja kan det vara en svensk by med
lite skog och annat ikring i n}got svenskt landskap. En massa problem
l|stes eftersom vi l{tt kunde prata med varandra om olika saker.

Det visade sig i diskussionen att det st|rsta problemet med kodandet i
spelet och med att beh}lla en bra och konsistent v{rld {r ett
kommunikationsproblem. Man beh|ver f|rmedla olika ideer till de nya
magikerna f|r att de skall kunna ta aktiv del i v{rldsskapandet och
det finns ganska d}liga m|jligheter f|r s}dant.

Avslutningsvis samlades vi f|r den officiella banketten som vi avnj|t
p} pizzeria-kebab Liz. Diskussionen under middagen drev iv{g ganska
l}ngt fr}n SvenskMUD men det var i alla fall en trevlig avslutning p}
en trevlig dag.

Linus
Aronsson, St, Gunnar, Povin, Nordgrimm, Hades, Meta, 
Testola, Storken, (Sn|skred)


1,, svmud,
Summary-line: 18-Apr      Gunnar@lysator.liu.se  46 #Faxanadu och andra fuskare.
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA10833; Sun, 18 Apr 1993 22:10:02 +0200 
Date: Sun, 18 Apr 1993 22:10:02 +0200
Message-Id: <199304182010.AA10833@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Faxanadu och andra fuskare.

*** EOOH ***
Date: Sun, 18 Apr 1993 22:10:02 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Faxanadu och andra fuskare.

Brev från Gunnar i SvenskMUD.
Kopia till:   st,radagast
Till:   linus

Borde du inte ha s{nkt lite annat p} Faxanadu? Egenskaper p} 100 hos en
spelare verkar lite osunt...

N{r jag tittade igenom loggfilerna uppt{ckte jag lite andra m{rkligheter.
Spelaren Bogey hade pl|tsligt f}tt v{ldigt h|g niv}. En titt i PLAYERS.old
visade att han mellan tv} noteringar g}tt fr}n niv} 5 till niv} 18.
Den 13 april 21:19 var han niv} 5 den 14 april 20:19 hade han n}tt niv} 18.
Egenskaperna var of|r{ndrat l}ga. En titt i ILLEGAL visade att
Armageddon hade anv{nt set_level under den aktuella tidsrymden.
Av mailadressen att d|mma {r det inte en testkarakt{r.

Av mitt snokande drog slutsatserna att PLAYERS har en l|ptid p} mindre
{n en vecka just nu. Efter tv} veckor finns det s}ledes ingen 
m|jlighet att utreda {ldre h{ndelser. Jag {r inte helt s{ker p} att
det {r riktigt tillr{ckligt.

ILLEGAL har {nnu st|rre brister. Information om att en spelare anv{nt 
set_level skulle vara mycket intressantare om man visste vem som
funktionen anv{nts p}. Det l}ter inte som om det m}ste vara alltf|r sv}rt
att implementera heller.

P} tal om Faxanadu drog han tydligen ig}ng sin show }tta minuter efter
att jag l{mnat spelet. Undrar om han avvaktade?

/Gunnar

Ps Jag har inte gjort n}gra }tg{rder ang}ende ovanst}ende mer {n detta
   brev {n s} l{nge. Ds


1,,
Summary-line: 19-Apr              MAILER-DAEMON  31 #Returned mail: Host unknown
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA00652; Mon, 19 Apr 1993 00:10:04 +0200 
Date: Mon, 19 Apr 1993 00:10:04 +0200
From: Mail Delivery Subsystem <MAILER-DAEMON>
Subject: Returned mail: Host unknown
Message-Id: <199304182210.AA00652@lysator.liu.se>

*** EOOH ***
Date: Mon, 19 Apr 1993 00:10:04 +0200
From: Mail Delivery Subsystem <MAILER-DAEMON>
Subject: Returned mail: Host unknown

   ----- Transcript of session follows -----
550 god@heaven.com... Host unknown

   ----- Unsent message follows -----
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA00650; Mon, 19 Apr 1993 00:10:04 +0200 
Date: Mon, 19 Apr 1993 00:10:04 +0200
Message-Id: <199304182210.AA00650@lysator.liu.se>
To: god@heaven.com
X-To-Mud-User: gud i Svenskmud.
From: Musashi@lysator.liu.se (Musashi i Svenskmud)
X-From-Mud-User: Musashi i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Jag bugar min inf|r Buddha

Brev från Musashi i SvenskMUD.
Till:   gud

[r du verkligen Gud? Har jag kontakt med Gud? Jag antar att n}gon
sk{mtar med mig. Men ifall att du {r Gud s} kanske du kan svara p} en
fr}ga?

Finns det n}gon m|jlighet f|r mig att logga in n}gonstans i Norrk|ping?
Det {r lite dyrt att ringa riks.... 
Fast det {r klart, jag undrar vad portot {r f|r ett brev till Gud! 8-)

Mvh. Miyamoto Musashi alias Johan Eliasson


1,,
Summary-line: 19-Apr    d92claar@und.ida.liu.se  13 #projektet
Received: from ida.liu.se (130.236.139.139; curofix.ida.liu.se) by lysator.liu.se (ALPHA-6.36/6.16) id AA06402; Mon, 19 Apr 1993 14:24:19 +0200 
Received: from und.ida.liu.se (astmatix.ida.liu.se) by ida.liu.se (5.65b/ida.minimaster-V1.0b6d5)
	id AA03857; Mon, 19 Apr 93 14:24:18 +0200
Received: from kolix (kolix-gw) by und.ida.liu.se (5.65b/ida.minimaster-V1.0b6d2)
	id AA23273; Mon, 19 Apr 93 14:24:16 +0200
From: Clarence Arnstedt <d92claar@und.ida.liu.se>
Received: from kol10 by kolix (4.1/ida.slave-V1.0b3)
	id AA13830; Mon, 19 Apr 93 14:24:14 +0200
Received: by kol10 (4.1/ida.slave-V1.0b3)
	id AA26080; Mon, 19 Apr 93 14:24:11 +0200
Date: Mon, 19 Apr 93 14:24:11 +0200
Message-Id: <9304191224.AA26080@kol10>
To: linus@lysator.liu.se
Subject: projektet

*** EOOH ***
From: Clarence Arnstedt <d92claar@und.ida.liu.se>
Date: Mon, 19 Apr 93 14:24:11 +0200
To: linus@lysator.liu.se
Subject: projektet

Vi har fatt allt ? enligt spec att fungera dock „r det lite svaert att veta
hur mycket som ska ner i documentationen.
Vi har analystenta pa fredag.
Kan vi v„nta med att redovisa doc. tills naesta vecka?
Maila till oss bada  -D92CLAAR  -D92JESBA

Clarence


1,,
Summary-line: 20-Apr      to: ceder@nada.kth.se  25 #Min post
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA00610; Tue, 20 Apr 1993 17:11:39 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA06255; Tue, 20 Apr 1993 17:11:36 +0200 
Date: Tue, 20 Apr 1993 17:11:36 +0200
From: linus@lysator.liu.se
Message-Id: <199304201511.AA06255@bodil.lysator.liu.se>
To: ceder@nada.kth.se
In-Reply-To: Testola i Svenskmud's message of Tue, 20 Apr 1993 12:10:12 +0200 <199304201010.AA11033@lysator.liu.se>
Subject: Min post

*** EOOH ***
Date: Tue, 20 Apr 1993 17:11:36 +0200
From: linus@lysator.liu.se
To: ceder@nada.kth.se
In-Reply-To: Testola i Svenskmud's message of Tue, 20 Apr 1993 12:10:12 +0200 <199304201010.AA11033@lysator.liu.se>
Subject: Min post

Fel vid l{sandet? Skumt, jag trodde bara det var vid skrivandet som
det kunde bli problem.

Observera att han st}r i /spelare/testola/sj_post.c. Den har inte
tillst}nd att skriva i /etc/post_dir.

Du borde ist{llet fixa s} att man g}r in i /rum/sj|tuna/sj_post.c
ist{llet. Den f}r lov att skriva i /etc/post_dir. Nu har du pillat
bort s} att den inte {r en l{nk l{ngre men jag g|r om den som en
syml{nk till din sj_post.c (det inneb{r att det {r precis samma fil
men om du inifr}n muddet refererar till den med namnet
/rum/sj|tuna/sj_post.c s} f}r den g|ra mer saker.)
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Summary-line: 21-Apr           marsj@ida.liu.se  19 #add-log.el
Received: from ida.liu.se (130.236.139.139; curofix.ida.liu.se) by lysator.liu.se (ALPHA-6.36/6.16) id AA03841; Wed, 21 Apr 1993 12:32:03 +0200 
Received: from obelix by ida.liu.se (5.65b/ida.minimaster-V1.0b6d5)
	id AA05194; Wed, 21 Apr 93 12:31:55 +0200
From: Martin Sjolin <marsj@ida.liu.se>
Received: from obel27 by obelix (5.65b/ida.slave-V1.0b3)
	id AA23814; Wed, 21 Apr 93 12:31:54 +0200
Received: by obel27 (5.65b/ida.slave-V1.0b3)
	id AA01821; Wed, 21 Apr 93 12:31:51 +0200
Date: Wed, 21 Apr 93 12:31:51 +0200
Message-Id: <9304211031.AA01821@obel27>
To: Linus Tolke <linus@lysator.liu.se>
Subject: add-log.el

*** EOOH ***
From: Martin Sjolin <marsj@ida.liu.se>
Date: Wed, 21 Apr 93 12:31:51 +0200
To: Linus Tolke <linus@lysator.liu.se>
Subject: add-log.el

Hej Linus,

har nu hackat (1h) klart en modifiering av add-log.el
s} att den letar efter ChangeLog i aktuell dir,
sedan i |verliggande etc {nda till rooten, och om
den inte finns, s} skapas (by default) en ChangeLog
i aktuellt dir. Namnet p} filen prefixas ocks} med
relativ path i f|rh}llande till ChangeLog ...

Finns det ingen funktion i elisp f|r att leta efter
en delstr{ng i en str{ng (ej i en buffer?)

msj


1,,
Summary-line: 22-Apr      svedja@lysator.liu.se   9 #Underskrift
Received: from lysita.lysator.liu.se (130.236.254.153) by lysator.liu.se (ALPHA-6.36/6.16) id AA04790; Thu, 22 Apr 1993 17:28:19 +0200 
Received: by lysita.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA22316; Thu, 22 Apr 1993 17:28:02 +0200
          (unknown)
Date: Thu, 22 Apr 1993 17:28:02 +0200
From: svedja@lysator.liu.se
Message-Id: <199304221528.AA22316@lysita.lysator.liu.se>
To: linus@lysator.liu.se
Subject: Underskrift

*** EOOH ***
Date: Thu, 22 Apr 1993 17:28:02 +0200
From: svedja@lysator.liu.se
To: linus@lysator.liu.se
Subject: Underskrift

Vaentar fortfarande pa din underskrift pa mina labbpapper. Det handlar ju bara om 4 (kanske lite mera) underskrifter so att jag kan laemna in paprena .

 MVH Dejan.


1,,
Summary-line: 24-Apr     Testola@lysator.liu.se  26 #Jag vill flytta till Sj|tuna, sa Mirar, det fungerar inte
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA13043; Sat, 24 Apr 1993 16:10:03 +0200 
Date: Sat, 24 Apr 1993 16:10:03 +0200
Message-Id: <199304241410.AA13043@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Testola@lysator.liu.se (Testola i Svenskmud)
Reply-To: ceder@nada.kth.se (Testola i Svenskmud)
X-From-Mud-User: Testola i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Jag vill flytta till Sj|tuna, sa Mirar, det fungerar inte

*** EOOH ***
Date: Sat, 24 Apr 1993 16:10:03 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Testola@lysator.liu.se (Testola i Svenskmud)
Reply-To: ceder@nada.kth.se (Testola i Svenskmud)
X-From-Mud-User: Testola i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Jag vill flytta till Sj|tuna, sa Mirar, det fungerar inte

Brev från Testola i SvenskMUD.
Till:   linus

Hej,
min tid b|rjar bli knapp kanh{nda. Jag arbetar en m}nad till h{r p} KTH.
D{refter {r min tid i SvenskMUD all tills vidare i varje fall. Mirar
anm{rkte p} min post ocks}... Har inte haft tid att fatta hur det fungerar
men kanske buggar den fortfarande. Om ni vill att Sj|tuna skall bli en
by precis som Strandhamn och Muddevall (:-) s} har t{nkte jag l{mpa
|ver det speltekniska ansvaret f|r det p} n}gont av er som h}ller p}
med dylikt. Tiden f|r min del {r {nd} snart |ver som sagt. Fr}gan {r vem
som kan ta hand om min v{rld medan {r {r borta.

Allt f|r nu...
Hej fr}n
Testola


1,,
Summary-line: 26-Apr      Gunnar@lysator.liu.se  26 #Magikerlistan
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA11487; Mon, 26 Apr 1993 14:10:04 +0200 
Date: Mon, 26 Apr 1993 14:10:04 +0200
Message-Id: <199304261210.AA11487@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Magikerlistan

*** EOOH ***
Date: Mon, 26 Apr 1993 14:10:04 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
To: leif@lysator.liu.se
X-To-Mud-User: st i Svenskmud.
To: johan@manwe.sunet.se
X-To-Mud-User: radagast i Svenskmud.
From: Gunnar@lysator.liu.se (Gunnar i Svenskmud)
Reply-To: gunnar@lysator.liu.se (Gunnar i Svenskmud)
X-From-Mud-User: Gunnar i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Magikerlistan

Brev från Gunnar i SvenskMUD.
Kopia till:   st,radagast
Till:   linus

Pontus har just nu 57652 kommandon p} magikerlistan eller 30%.
Det {r s{kerligen n}got som {r fel i omr}det som g|r att spelarna
{r v{ldigt ben{gna att g|ra v{ldigt mycket just d{r till exempel
p} grund av att det l|nar sig alldeles f|r bra.

Jag har dock inte lyckats finna vad det kan r|ra sig om.

/Gunnar


1,,
Summary-line: 27-Apr   to: gt92agu@txfs1.hfb.se  91 #Datorförening
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA23932; Tue, 27 Apr 1993 03:48:02 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA11910; Tue, 27 Apr 1993 03:47:45 +0200 
Date: Tue, 27 Apr 1993 03:47:45 +0200
From: linus@lysator.liu.se
Message-Id: <199304270147.AA11910@bodil.lysator.liu.se>
To: gt92agu@txfs1.hfb.se
In-Reply-To: Anders Gustavsson - HFB T gt92's message of Mon, 15 Mar 93 19:20:59 +0100 <9303151820.AA04694@t.hfb.se>
Subject: Datorförening

*** EOOH ***
Date: Tue, 27 Apr 1993 03:47:45 +0200
From: linus@lysator.liu.se
To: gt92agu@txfs1.hfb.se
In-Reply-To: Anders Gustavsson - HFB T gt92's message of Mon, 15 Mar 93 19:20:59 +0100 <9303151820.AA04694@t.hfb.se>
Subject: Datorförening

Jag gick igenom mina gamla brev och hittade ditt.

Jag skickade det vidare till lysator@lysator.liu.se f|r att det kom
inte riktigt r{tt i alla fall. Jag {r bara medlem, inte ens med i
styrelsen.

   * Hur har Ni tiggt till Er datorer och vart/till vem man ska v_nda sig i denna fr_ga?

Jag vet inte. Lysator startades ursprungligen f|r att kunna hantera en
donation fr}n datasaab. P} senare }r har vi f}tt mycket olika maskiner
av olika f|retag. Jag har inte varit direkt engagerad i sj{lva
tiggandet men jag tror att man g|r p} f|ljande vis:
- hittar ett f|retag som skall byta ut sin dator mot en modernare
- ser till att f} den gamle (vanligt {r att f|retaget l{mnar in sin
  gamla dator vid uppdatering och f}r d} en saftig rabatt av
  leverant|ren av den nya datorn. D} kan man prata med leverant|ren,
  och se till att man f}r den av dem och kan h{mta den p} f|retaget i
  alla fall.
  

   * Kan man ans_ka om bidrag eller dylikt f_r denna verksamhet
     (igentligen: hur finasierar ni f_reningen)?

Vi finansierar f|reningen mha medlemsavgifter, f|r tillf{llet 128
kronor per }r. Det g}r till administrativa utgifter, backupband och
v}r medlemstidning.

Ink|p av maskiner, diskar, hus, mm betalar vi inte, vi k|per ingenting
eller ocks} betalas det av institutioner, f|retag.

   * Hur har ni gjort med programvaror (f_tt av skolan, k_pt egna licenser etc)?

Vi lever helt i unix-v{rlden. Vi anv{nder n{stan uteslutande
programvara som {r gratis, skolan har licenser (SunOS) eller f|r vilka
vi f}tt licenser (BSD).

   * Har Ni n_gra stadgar och m_tesprotokoll som Du kan mail:a upp till mig?

Ja, vi har stadgarna liggande. Jag skickar dem i n{sta brev.

   * Har Du n_gra _vriga tips eller r_d?

Nej, jo.

Det finns ett nordiskt samarbete mellan universitetsdatorf|reningar i
norden som heter nucc. Det inskr{nker sig f|r n{rvarande till en
nyhetsgrupp: nordunet.nucc och en }rlig sammankomst nuccc.

N{sta nuccc {r i b|rjan av sommaren och det organiseras av Stacken
(kth) den 11-13 juni.

Anm{lan och fr}gor troligtvis till nuccc-93@stacken.kth.se.

   Som du f_rst_r av ovanst_ende s_ har jag dragit ig_ng en en
   datorf_rening h_r p_ h_gskolan Falun/Borl_nge. Jag hoppas att vi
   kan n_gon form av utbyte i framtiden. En g_stf_rel_sning om t ex
   brister i UNIX av n_gon i lysator? Beltalningen skulle givetvis
   vara "studentbetonad": resan betald, _vernattning hos n_gon i
   f_reningen och gratis _l p_ k_ren...

L}ter sp{nnande men de finns nog ingen som k{nner till n}gra brister i
Unix. Vi {r alla unix-fr{lsta! Men n}got annat i den stilen kanske g}r
att ordna. Framf|r allt, se till att n}gon fr}n din f|rening }ker p}
nuccc!

V}r verksamhet {r i princip indelad i olika fack.
a) personer som pratar med f|retag, raggar maskiner och }ker och
   h{mtar dem.
b) personer som underh}ller maskinerna, stoppar i nya diskar, tar
   backup, mm.
c) personer som jobbar med olika projekt, lyskom, mud, egna projekt.
d) personer som spelar spel, nethack, mud eller pratar skit, irc, lyskom.

Vi har {ven en viss kursverksamhet, jag h|ll f|r ett }r sedan en
emacs-lisp kurs, Per Cederqvist <ceder@lysator.liu.se> h|ll en C-kurs,
Lars Aronsson <aronsson@lysator.liu.se> h|ll i b|rjan av denna
terminen en unix-kurs som nu har blivit Linux-kurs. Mattias Wingstedt
<wing@lysator.liu.se> h}ller just nu en LPCkurs.
--
	/Linus, Gud i SvenskMUD
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Summary-line: 27-Apr  to: kpc@ptolemy.arc.nasa. 465 #Virtual groups
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA24807; Tue, 27 Apr 1993 04:08:33 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA11987; Tue, 27 Apr 1993 04:08:16 +0200 
Date: Tue, 27 Apr 1993 04:08:16 +0200
From: linus@lysator.liu.se
Message-Id: <199304270208.AA11987@bodil.lysator.liu.se>
To: kpc@ptolemy.arc.nasa.gov
In-Reply-To: Kimball Collins's message of Wed, 14 Apr 93 19:05:08 PDT <9304150205.AA14752@ptolemy.arc.nasa.gov>
Subject: Virtual groups

*** EOOH ***
Date: Tue, 27 Apr 1993 04:08:16 +0200
From: linus@lysator.liu.se
To: kpc@ptolemy.arc.nasa.gov
In-Reply-To: Kimball Collins's message of Wed, 14 Apr 93 19:05:08 PDT <9304150205.AA14752@ptolemy.arc.nasa.gov>
Subject: Virtual groups

I was cleaning my RMAIL file and found this:

   have you been using your virtual groups code and still like it?  it
   might be a good thing to make part of gnus.  would you be willing to
   let me be a beta tester?

I am still using it, and still liking it but my ambitions are low
regarding putting any work into it. If you want a copy to be able to
test it I can send you one. (The end of this letter).
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)
---
;; Most of this code is just stolen from the gnus 3.14 and just simply modified
;; Gnu copyleft apply.
;; 
;; The modifying of the code is done by Linus Tolke <linus@lysator.liu.se>
;;
;; Instructions:
;; * while in newsgroup selection mode you can type m regexp to open a
;;   "virtual group" with all groups matching.
;; 
;; Bugs:
;; * You can not specify a regexp that is a groupname also. This will choose 
;;   the group instead.
;; * killfiles are not tested, perhaps they will work.
;;
;; Possible future enhancements:
;; * save the favorite regexp and show them (along with number of unreads)
;;   in the buffer for ease of use.
;; * special hook only run when entering a "virtual" group.
;;

(require 'gnus)

(defvar gnus-reading-matching-groups nil
  "Non-nil if we are entering a virtual group.")

(defvar gnus-virtual-newsgroup-mapping nil
  "Mapping from virtual number to newsgroup and article name.")

(define-key gnus-Group-mode-map "m" 'gnus-Group-read-matching-groups)

(defun gnus-Group-read-matching-groups (match)
  "Reads the newsgroup matching MATCH (regexp)"
  (interactive "sMatching (regexp): ")
  (let ((gnus-reading-matching-groups match))
    (gnus-Subject-read-group
     match nil
     nil)))

(defun gnus-select-newsgroup (group &optional show-all)
  "Select newsgroup GROUP.
If group is not an existing group the we think it is a virtual group.
If optional argument SHOW-ALL is non-nil, all of articles in the group
are selected."
  (if (gnus-request-group group)
      (let ((articles nil))
	(setq gnus-newsgroup-name group)
	(setq gnus-newsgroup-unreads
	      (gnus-uncompress-sequence
	       (nthcdr 2 (gnus-gethash group gnus-unread-hashtb))))
	(cond (show-all
	       ;; Select all active articles.
	       (setq articles
		     (gnus-uncompress-sequence
		      (nthcdr 2 (gnus-gethash group gnus-active-hashtb)))))
	      (t
	       ;; Select unread articles only.
	       (setq articles gnus-newsgroup-unreads)))
	;; Require confirmation if selecting large newsgroup.
	(setq gnus-newsgroup-unselected nil)
	(if (not (numberp gnus-large-newsgroup))
	    nil
	  (let ((selected nil)
		(number (length articles)))
	    (if (> number gnus-large-newsgroup)
		(progn
		  (condition-case ()
		      (let ((input
			     (read-string
			      (format
			       "How many articles from %s (default %d): "
			       gnus-newsgroup-name number))))
			(setq selected
			      (if (string-equal input "")
				  number (string-to-int input))))
		    (quit
		     (setq selected 0)))
		  (cond ((and (> selected 0)
			      (< selected number))
			 ;; Select last N articles.
			 (setq articles (nthcdr (- number selected) articles)))
			((and (< selected 0)
			      (< (- 0 selected) number))
			 ;; Select first N articles.
			 (setq selected (- 0 selected))
			 (setq articles (copy-sequence articles))
			 (setcdr (nthcdr (1- selected) articles) nil))
			((zerop selected)
			 (setq articles nil))
			;; Otherwise select all.
			)
		  ;; Get unselected unread articles.
		  (setq gnus-newsgroup-unselected
			(gnus-set-difference gnus-newsgroup-unreads articles))
		  ))
	    ))
	;; Get headers list.
	(setq gnus-newsgroup-headers (gnus-retrieve-headers articles))
	;; UNREADS may contain expired articles, so we have to remove
	;;  them from the list.
	(setq gnus-newsgroup-unreads
	      (gnus-intersection gnus-newsgroup-unreads
				 (mapcar
				  (function
				   (lambda (header)
				     (nntp-header-number header)))
				  gnus-newsgroup-headers)))
	;; Marked article must be a subset of unread articles.
	(setq gnus-newsgroup-marked
	      (gnus-intersection (append gnus-newsgroup-unselected
					 gnus-newsgroup-unreads)
				 (cdr (assoc group gnus-marked-assoc))))
	;; First and last article in this newsgroup.
	(setq gnus-newsgroup-begin
	      (if gnus-newsgroup-headers
		  (nntp-header-number (car gnus-newsgroup-headers))
		0
		))
	(setq gnus-newsgroup-end
	      (if gnus-newsgroup-headers
		  (nntp-header-number
		   (gnus-last-element gnus-newsgroup-headers))
		0
		))
	;; File name that an article was saved last.
	(setq gnus-newsgroup-last-rmail nil)
	(setq gnus-newsgroup-last-mail nil)
	(setq gnus-newsgroup-last-folder nil)
	(setq gnus-newsgroup-last-file nil)
	;; Reset article pointer etc.
	(setq gnus-current-article nil)
	(setq gnus-current-headers nil)
	(setq gnus-current-history nil)
	(setq gnus-have-all-headers nil)
	(setq gnus-last-article nil)
	;; Clear old hash tables for the variable gnus-newsgroup-headers.
	(gnus-clear-hashtables-for-newsgroup-headers)
	;; GROUP is successfully selected.
	t
	)
    ;; Probably virtual group.
    (let ((virtualgroups (gnus-find-virtual-groups group)))
      (if (and gnus-reading-matching-groups
	       virtualgroups)
	  (let ((articles nil)
		thisgroup
		headers
		(number 1))
	    (setq gnus-newsgroup-headers nil)
	    (setq gnus-newsgroup-marked nil)
	    (setq gnus-virtual-newsgroup-mapping nil)
	    (setq gnus-newsgroup-name (concat "Virtual: " group 
					      (format " (%d)"
						      (length virtualgroups))))
	    (while (and virtualgroups
			(gnus-request-group (car virtualgroups)))
	      (setq thisgroup (car virtualgroups))
	      (setq virtualgroups (cdr virtualgroups))
	      (setq gnus-newsgroup-unreads
		    (gnus-uncompress-sequence
		     (nthcdr 2 (gnus-gethash thisgroup gnus-unread-hashtb))))
	      (cond (show-all
		     ;; Select all active articles.
		     (setq articles
			   (gnus-uncompress-sequence
			    (nthcdr 2 (gnus-gethash group gnus-active-hashtb)))))
		    (t
		     ;; Select unread articles only.
		     (setq articles gnus-newsgroup-unreads)))
	      ;; Require confirmation if selecting large newsgroup.
	      (setq gnus-newsgroup-unselected nil)
	      (if (not (numberp gnus-large-newsgroup))
		  nil
		(let ((selected nil)
		      (number (length articles)))
		  (if (> number gnus-large-newsgroup)
		      (progn
			(condition-case ()
			    (let ((input
				   (read-string
				    (format
				     "How many articles from %s (default %d): "
				     thisgroup number))))
			      (setq selected
				    (if (string-equal input "")
					number (string-to-int input))))
			  (quit
			   (setq selected 0)))
			(cond ((and (> selected 0)
				    (< selected number))
			       ;; Select last N articles.
			       (setq articles (nthcdr (- number selected) articles)))
			      ((and (< selected 0)
				    (< (- 0 selected) number))
			       ;; Select first N articles.
			       (setq selected (- 0 selected))
			       (setq articles (copy-sequence articles))
			       (setcdr (nthcdr (1- selected) articles) nil))
			      ((zerop selected)
			       (setq articles nil))
			      ;; Otherwise select all.
			      )
			))
		  ))

	      ;; Marked article are removed
	      (let ((markedlist (cdr (assoc thisgroup gnus-marked-assoc))))
		(if markedlist
		    (setq articles
			  (apply 'append (mapcar 
					  (function (lambda (x) 
						      (if (memq x markedlist)
							  nil
							(list x))))
					  articles)))))
	      ;; Get headers list.
	      (setq headers (gnus-retrieve-headers articles))
	      (let ((checked nil))
		(while headers
		  (if (among-headers (car headers) gnus-newsgroup-headers)
		      nil
		    (setq checked (cons (car headers) checked)))
		  (setq headers (cdr headers)))
		(setq headers (nreverse checked)))
	      (setq gnus-newsgroup-headers
		    (append gnus-newsgroup-headers
			    (mapcar (function 
				     (lambda (header)
				       (setq gnus-virtual-newsgroup-mapping
					     (cons
					      (list number thisgroup 
						    (aref header 0))
					      gnus-virtual-newsgroup-mapping))
				       (aset header 0 number)
				       (setq number (1+ number))
				       header))
				    headers))))

	    (setq gnus-newsgroup-unreads
		  (mapcar (function (lambda (header)
				      (aref header 0)))
			  gnus-newsgroup-headers))
	    ;; First and last article in this newsgroup.
	    (setq gnus-newsgroup-begin 
		  (if gnus-newsgroup-headers
		      (nntp-header-number (car gnus-newsgroup-headers))
		    0
		    ))
	    (setq gnus-newsgroup-end
		  (if gnus-newsgroup-headers
		      (nntp-header-number
		       (gnus-last-element gnus-newsgroup-headers))
		    0
		    ))
	    ;; File name that an article was saved last.
	    (setq gnus-newsgroup-last-rmail nil)
	    (setq gnus-newsgroup-last-mail nil)
	    (setq gnus-newsgroup-last-folder nil)
	    (setq gnus-newsgroup-last-file nil)
	    ;; Reset article pointer etc.
	    (setq gnus-current-article nil)
	    (setq gnus-current-headers nil)
	    (setq gnus-current-history nil)
	    (setq gnus-have-all-headers nil)
	    (setq gnus-last-article nil)
	    ;; Clear old hash tables for the variable gnus-newsgroup-headers.
	    (gnus-clear-hashtables-for-newsgroup-headers)
	    ;; GROUP is successfully selected.
	    t
	    )))
    ))


(defun gnus-find-virtual-groups (match)
  "Returnerar en lista av matchande grupper."
  (let (list)
    (mapcar
     (function (lambda (pair)
		 (if (and (car (cdr pair))
			  (string-match match (car pair)))
		     (setq list (cons (car pair) list)))))
     gnus-newsrc-assoc)
    (nreverse list)))


(defun gnus-Group-prepare-line (info)
  "Return a string for the Newsgroup buffer from INFO.
INFO is an element of gnus-newsrc-assoc or gnus-killed-assoc."
  (if info
      (let* ((group-name (car info))
	     (unread-count
	      (or (nth 1 (gnus-gethash group-name gnus-unread-hashtb))
		  ;; Not in hash table, so compute it now.
		  (gnus-number-of-articles
		   (gnus-difference-of-range
		    (nth 2 (gnus-gethash group-name gnus-active-hashtb))
		    (nthcdr 2 info)))))
	     ;; This specifies the format of Group buffer.
	     (cntl "%s%s%5d: %s\n"))
	(format cntl
		;; Subscribed or not.
		(if (nth 1 info) " " "U")
		;; Has new news?
		(if (and (> unread-count 0)
			 (>= 0
			     (- unread-count
				(length
				 (cdr (assoc group-name gnus-marked-assoc))))))
		    "*" " ")
		;; Number of unread articles.
		unread-count
		;; Newsgroup name.
		group-name
		))
    ""))


(defun gnus-Article-prepare (article &optional all-headers)
  "Prepare ARTICLE in Article mode buffer.
If optional argument ALL-HEADERS is non-nil, all headers are inserted."
  (save-excursion
    (set-buffer gnus-Article-buffer)
    (let ((buffer-read-only nil)
	  (realarticle article))
      (erase-buffer)
      (if (cond ((string-match "^Virtual: " gnus-newsgroup-name)
		 ; The article number is local, find the newsgroup name and
		 ; article number in gnus-newsgroup-headers
		 (gnus-request-group 
		  (car (cdr (assoc article 
				   gnus-virtual-newsgroup-mapping))))
		 (setq realarticle
		       (car (cdr 
			     (cdr 
			      (assoc article
				     gnus-virtual-newsgroup-mapping)))))
		 (gnus-request-article realarticle))
		(t (gnus-request-article article)))
	  (progn
	    ;; Prepare article buffer
	    (insert-buffer-substring nntp-server-buffer)
	    (setq gnus-have-all-headers (or all-headers gnus-show-all-headers))
	    (if (and (numberp article)
		     (not (eq article gnus-current-article)))
		;; Seems me that a new article is selected.
		(progn
		  ;; gnus-current-article must be an article number.
		  (setq gnus-last-article gnus-current-article)
		  (setq gnus-current-article article)
;;		  (setq gnus-current-headers
;;			(gnus-find-header-by-number gnus-newsgroup-headers
;;						    gnus-current-article))
		  (setq gnus-current-headers
			(gnus-get-header-by-number gnus-current-article))
		  ;; Clear articles history only when articles are
		  ;; retrieved by article numbers.
		  (setq gnus-current-history nil)
		  (run-hooks 'gnus-Mark-article-hook)
		  ))
	    ;; Hooks for modifying contents of the article. This hook
	    ;; must be called before being narrowed.
	    (run-hooks 'gnus-Article-prepare-hook)
	    ;; Delete unnecessary headers.
	    (or gnus-have-all-headers
		(gnus-Article-delete-headers))
	    ;; Do page break.
	    (goto-char (point-min))
	    (if gnus-break-pages
		(gnus-narrow-to-page))
	    ;; Next function must be called after setting
	    ;;  `gnus-current-article' variable and narrowed to page.
	    (gnus-Article-set-mode-line)
	    )
	(if (numberp article)
	    (gnus-Subject-mark-as-read article))
	(ding) (message "No such article (may be canceled)"))
      )))

(defun gnus-mark-as-read-by-xref
  (group headers unreads &optional subscribed-only)
  "Mark articles as read using cross references and return updated newsgroups.
Arguments are GROUP, HEADERS, UNREADS, and optional SUBSCRIBED-ONLY.
If newsgroup is a virtual newsgroup then return all newsgroups."
  (let ((xref-list nil)
	(header nil)
	(xrefs nil)			;One Xref: field info.
	(xref nil)			;(NEWSGROUP . ARTICLE)
	(gname nil)			;Newsgroup name
	(article nil))			;Article number
    (while headers
      (setq header (car headers))
      (if (memq (nntp-header-number header) unreads)
	  ;; This article is not yet marked as read.
	  nil
	(setq xrefs (gnus-parse-xref-field (nntp-header-xref header)))
	(if (string-match "^Virtual: " gnus-newsgroup-name)
	    ; We have a virtual group. Enter the group in xrefs first.
	    ; Yiiieeeekkk!
	    (let* ((entry (assoc (nntp-header-number header)
				 gnus-virtual-newsgroup-mapping))
		   (newsgroup (car (cdr entry)))
		   (artno (car (cdr (cdr entry)))))
	      (setq xrefs (cons (cons newsgroup artno) xrefs))))
	;; For each cross reference info. in one Xref: field.
	(while xrefs
	  (setq xref (car xrefs))
	  (setq gname (car xref))	;Newsgroup name
	  (setq article (cdr xref))	;Article number
	  (or (string-equal group gname) ;Ignore current newsgroup.
	      ;; Ignore unsubscribed newsgroup if requested.
	      (and subscribed-only
		   (not (nth 1 (assoc gname gnus-newsrc-assoc))))
	      ;; Ignore article marked as unread.
	      (memq article (cdr (assoc gname gnus-marked-assoc)))
	      (let ((group-xref (assoc gname xref-list)))
		(if group-xref
		    (if (memq article (cdr group-xref))
			nil		;Alread marked.
		      (setcdr group-xref (cons article (cdr group-xref))))
		  ;; Create new assoc entry for GROUP.
		  (setq xref-list (cons (list gname article) xref-list)))
		))
	  (setq xrefs (cdr xrefs))
	  ))
      (setq headers (cdr headers)))
    ;; Mark cross referenced articles as read.
    (gnus-mark-xrefed-as-read xref-list)
    ;;(message "%s %s" (prin1-to-string unreads) (prin1-to-string xref-list))
    ;; Return list of updated group name.
    (mapcar (function car) xref-list)
    ))


(defun among-headers (header list)
  "Returns non-nil if HEADER is in LIST of headers."
  (let ((found nil))
    (while (and list
		(not found))
      (if (string= (nntp-header-id header)
		   (nntp-header-id (car list)))
	  (setq found t))
      (setq list (cdr list)))
    found))


1,,
Summary-line: 27-Apr  o: cardell@lysator.liu.se  16 #swedish-minor & visa-svenska
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA24944; Tue, 27 Apr 1993 04:11:27 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA11992; Tue, 27 Apr 1993 04:11:11 +0200 
Date: Tue, 27 Apr 1993 04:11:11 +0200
From: linus@lysator.liu.se
Message-Id: <199304270211.AA11992@bodil.lysator.liu.se>
To: cardell@lysator.liu.se
In-Reply-To: cardell@lysator.liu.se's message of Sat, 17 Apr 1993 19:45:55 +0200 <199304171745.AA02384@bodil.lysator.liu.se>
Subject: swedish-minor & visa-svenska

*** EOOH ***
Date: Tue, 27 Apr 1993 04:11:11 +0200
From: linus@lysator.liu.se
To: cardell@lysator.liu.se
In-Reply-To: cardell@lysator.liu.se's message of Sat, 17 Apr 1993 19:45:55 +0200 <199304171745.AA02384@bodil.lysator.liu.se>
Subject: swedish-minor & visa-svenska

   jag hackar i C, s} det fungerar bra, det {r bara representationen p}
   sk{rmen jag vill }t nu.
K|r du screen? => gl|m att visa b}da samtidigt d}.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Received: from ptolemy.arc.nasa.gov (128.102.114.134; ptolemy-ethernet.arc.nasa.gov) by lysator.liu.se (ALPHA-6.36/6.16) id AA01945; Tue, 27 Apr 1993 06:54:31 +0200 
Received: from zog.arc.nasa.gov by ptolemy.arc.nasa.gov (4.1/) id <AA22169>; Mon, 26 Apr 93 21:56:20 PDT
Date: Mon, 26 Apr 93 21:56:20 PDT
Message-Id: <9304270456.AA22169@ptolemy.arc.nasa.gov>
Received: by zog.arc.nasa.gov (4.1/SMI-4.1)
	id AA10655; Mon, 26 Apr 93 21:56:31 PDT
From: Kimball Collins <kpc@ptolemy.arc.nasa.gov>
Sender: kpc@ptolemy.arc.nasa.gov
To: linus@lysator.liu.se
Subject: virtual groups
Reply-To: kpc@ptolemy.arc.nasa.gov

*** EOOH ***
Date: Mon, 26 Apr 93 21:56:20 PDT
From: Kimball Collins <kpc@ptolemy.arc.nasa.gov>
Sender: kpc@ptolemy.arc.nasa.gov
To: linus@lysator.liu.se
Subject: virtual groups
Reply-To: kpc@ptolemy.arc.nasa.gov

i am amazed at how well it works and think it would be a good
candidate for gk.

the only thing i see missing is that for some reason marked-as-unread
articles (i call them "ticked" articles; the ones that are permanently
marked as unread with "-") are not displayed like they are in ordinary
groups.

and the only feature missing from a complete scheme is one you mentioned:

;; * save the favorite regexp and show them (along with number of unreads)
;;   in the buffer for ease of use.

bravo.  do you have any idea why the "-" articles don't show up?

--
Help preserve the U.S. Bill of Rights!  Unpopular, private -- it's true,
even RANDOM -- speech is necessarily free.  Random sigs mean YOU concur!
{FN=JcOqKd,v[=s.'Z]5qxIee@mDX=0AYb,sR|SAX}+qcy0=JPA4Ivc,Qd4Y@X#rq%]`7xB'
LPwz1GC*4x?iFF*xJDBppO7]b|W?akh{bE1<Kc(9Fc[2"2Z4<9_wP\pbq1,a%K)|QO<6rqi~


1,,
Received: from varg.lysator.liu.se (130.236.254.151) by lysator.liu.se (ALPHA-6.36/6.16) id AA15542; Tue, 27 Apr 1993 13:27:24 +0200 
Received: by varg.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA05746; Tue, 27 Apr 93 13:26:53 +0200
          (unknown)
Date: Tue, 27 Apr 93 13:26:53 +0200
From: svmud@lysator.liu.se
Message-Id: <9304271126.AA05746@varg.lysator.liu.se>
To: svmud@lysator.liu.se
Subject: SvenskMUDs omstart nummer 14 gick ner. Exit status: 2

*** EOOH ***
Date: Tue, 27 Apr 93 13:26:53 +0200
From: svmud@lysator.liu.se
To: svmud@lysator.liu.se
Subject: SvenskMUDs omstart nummer 14 gick ner. Exit status: 2

'          do_it' in 'stdobj/magiker/brain.c' ('stdobj/magiker/brain#59325')line 4053
'       do_macro' in 'stdobj/magiker/brain.c' ('stdobj/magiker/brain#59325')line 3850
'     my_command' in 'stdobj/magiker/brain.c' ('stdobj/magiker/brain#59325')line 125
'       force_us' in '        obj/living.c' ('    obj/player#59323')line 902
'        command' in '        std/object.c' ('    obj/player#59323')line 91
'           load' in '        obj/player.c' ('    obj/player#59323')line 2893
Totally out of MEMORY.
'          reset' in 'spelare/gwentar/trollskog/trollskog85.c' ('spelare/gwentar/trollskog/trollskog85')line 36
Exit status:  2
Thu Apr 22 14:37:22 1993 Game shutdown by kiwi(en massa buggar!)
'           load' in '        obj/player.c' ('    obj/player#59323')line 2893
Error in loading object
program: obj/player.c, object: obj/player#59323 line 2893
'          do_it' in 'stdobj/magiker/brain.c' ('stdobj/magiker/brain#59325')line 4053
'       do_macro' in 'stdobj/magiker/brain.c' ('stdobj/magiker/brain#59325')line 3850
'     my_command' in 'stdobj/magiker/brain.c' ('stdobj/magiker/brain#59325')line 125
'       force_us' in '        obj/living.c' ('    obj/player#59323')line 902
'        command' in '        std/object.c' ('    obj/player#59323')line 91
'           load' in '        obj/player.c' ('    obj/player#59323')line 2893
'          reset' in 'spelare/gwentar/trollskog/trollskog85.c' ('spelare/gwentar/trollskog/trollskog85')line 36
       F UID   PID  PPID CP PRI NI  SZ  RSS WCHAN        STAT TT  TIME COMMAND
   80003   0     0     0  0 -25  0   0    0 runout       D    ?  11:20 swapper
20088001   0     1     0  1   5  0  56   40 child        S    ?   0:12 /sbin/init -
   80003   0     2     0  0 -24  0   0    0 child        D    ?  59:58 pagedaemon
   88000   0    55     1  0   1  0  80    0 select       IW   ?   1:57 portmap
   88000   3    58     1  1   1  0  40    0 select       IW   ?   0:00 ypbind
   88000   0    60     1  0   1  0  56    0 select       IW   ?   0:01 keyserv
   88001   0    73     1  0   1  0  24    0 nfs_dnlc     S    ?   4:03  (biod)
   88001   0    74     1  0   1  0  24    0 nfs_dnlc     S    ?   3:58  (biod)
   88001   0    75     1  0   1  0  24    0 nfs_dnlc     S    ?   4:06  (biod)
   88001   0    76     1  0   1  0  24    0 nfs_dnlc     S    ?   4:05  (biod)
   88000   0    86     1  0   1  0  64    0 select       IW   ?   0:25 syslogd
  488001   0    98     1  0   1  0  40    0 socket       I    ?  24:39  (nfsd)
   88000   0    99     1  0   1  0  72    0 select       IW   ?   3:26 rpc.mountd -n
   88001   0   102    98  0   1  0  40    0 socket       I    ?  24:32  (nfsd)
   88001   0   103    98  0   1  0  40    0 socket       I    ?  24:58  (nfsd)
   88001   0   104    98  0   1  0  40    0 socket       I    ?  24:49  (nfsd)
   88001   0   105    98  0   1  0  40    0 socket       I    ?  24:35  (nfsd)
   88001   0   107    98  0   1  0  40    0 socket       I    ?  25:02  (nfsd)
   88001   0   108    98  0   1  0  40    0 socket       I    ?  24:37  (nfsd)
   88001   0   109    98  0   1  0  40    0 socket       I    ?  24:50  (nfsd)
   88000   0   110     1  0   1  0 144    0 select       IW   ?   0:05 rpc.lockd
   88000   0   111     1  0   1  0  72    0 select       IW   ?   0:00 rpc.statd
   80200   0   127     1  1  15  0  24    0 kernelmap    IW   ?  93:34 update
  488000   0   130     1  0   1  0  64    0 usrpt        IW   ?   0:40 cron
  488001   0   136     1  0   1  0  48   80 socket       I    ?  26:56 in.rwhod
   88000   0   141     1  0   1  0  56    0 select       IW   ?   0:00 /usr/lib/lpd
   88000   0   770     1  0   1  0 224    0 select       IW   ?   1:04 /usr/X11/bin/xdm
20088000   0  1614 11892  0   1  0  64    0 select       IW   ?   0:00 in.rshd
20488401 124  1615  1614 32  33  0   0    0              Z    ?   0:00 <defunct>
20080000 124  1663     1  6   1  0  56    0 select       IW   ?   0:00 rsh -n robert /usr/X11/bin/xterm -geometry 110x63+0-2 -n irc -j -e /usr/local/bin/irc
   80401 124  1665  1663  1  32  0   0    0              Z    ?   0:00 <defunct>
200880014893  4814     1 10   1  012368 2496 select       S    ? 131:05 /home/lyskom/bin/lyskomd -p4894 -P4895 -D/home/lyskom
20080001  21  5741 23311 20  30  0 224  528              R    ?   0:00 ps alxww
20088001  21  5742 23311  8   1  0  72  368 socket       S    ?   0:00 Mail -s SvenskMUDs omstart nummer 14 gick ner. Exit status: 2 svmud
   88000   0 11892     1  0   1  0  56    0 select       IW   ?   5:56 inetd
20088201  21 23311     1  2  15  0  72  184 kernelmap    S    ?   0:03 /usr/bin/csh -b bin/restartmud
   88001   0 24513     1  1   1  0 120  256 select       S    ?   9:12 automount
80088001 259 24649     1  8   1  0  88  160 select       S    ?  44:35 lpmud/bin/mudfingerd -l
20088000   0  4720     1 11   3  0  56    0 usrpt        IW   co  0:00 - std.9600 console (getty)


1,,
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA23944; Tue, 27 Apr 1993 17:18:51 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA13951; Tue, 27 Apr 1993 17:18:34 +0200 
Date: Tue, 27 Apr 1993 17:18:34 +0200
From: linus@lysator.liu.se
Message-Id: <199304271518.AA13951@bodil.lysator.liu.se>
To: kpc@ptolemy.arc.nasa.gov
In-Reply-To: <9304270456.AA22169@ptolemy.arc.nasa.gov> (kpc@ptolemy.arc.nasa.gov)
Subject: virtual groups

*** EOOH ***
Date: Tue, 27 Apr 1993 17:18:34 +0200
From: linus@lysator.liu.se
To: kpc@ptolemy.arc.nasa.gov
In-Reply-To: <9304270456.AA22169@ptolemy.arc.nasa.gov> (kpc@ptolemy.arc.nasa.gov)
Subject: virtual groups

   i am amazed at how well it works and think it would be a good
   candidate for gk.
gk?

   the only thing i see missing is that for some reason marked-as-unread
   articles (i call them "ticked" articles; the ones that are permanently
   marked as unread with "-") are not displayed like they are in ordinary
   groups.
In an earlier version I forgot to reset the list of marked-as-unread
so if I unmarked a few articles in a virtual group the next time I
enter a virtual group the articles that happened to get the same
number would show up as unmarked.

The problem is not that the unmarked articles wont show up but that
they are not saved anywhere.

These cannot be saved as numbers (as are unmarked articles in ordinary
groups) because the numbers are generated anew everytime you enter a
virtual group. They have to be saved as Message-IDs or possibly group
and number.

I took the easy way out when writing the code and just forgot about them.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA27791; Tue, 27 Apr 1993 19:01:11 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA14471; Tue, 27 Apr 1993 19:00:55 +0200 
Date: Tue, 27 Apr 1993 19:00:55 +0200
From: linus@lysator.liu.se
Message-Id: <199304271700.AA14471@bodil.lysator.liu.se>
To: bellman@lysator.liu.se
Subject: emacs@lysator, pathar s} att jag blir yr i huvudet.

*** EOOH ***
Date: Tue, 27 Apr 1993 19:00:55 +0200
From: linus@lysator.liu.se
To: bellman@lysator.liu.se
Subject: emacs@lysator, pathar s} att jag blir yr i huvudet.

Jag {ndrade:

  /usr/gnu/src/emacs-18.59-gayle/src:
  -rw-rw-r--   1 bellman  local         827 Apr  5 18:29 paths.h

till:

/* the extra search path for programs to invoke.
 This is appended to whatever the PATH environment variable says
 to set the Lisp variable exec-path and the first file namein it
  sets the Lisp variable exec-directory.  */
#define PATH_EXEC "/usr/gnu/lib/emacs/etc"
			    ^^^
s} att man slipper 
Warning: executable/documentation dir (/usr/gnu/var/emacs/etc/) does not exist.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA01348; Tue, 27 Apr 1993 20:17:19 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA15549; Tue, 27 Apr 1993 20:17:01 +0200 
Date: Tue, 27 Apr 1993 20:17:01 +0200
From: linus@lysator.liu.se
Message-Id: <199304271817.AA15549@bodil.lysator.liu.se>
To: bellman@lysator.liu.se
Subject: Att fixas någon gång när du får tid

*** EOOH ***
Date: Tue, 27 Apr 1993 20:17:01 +0200
From: linus@lysator.liu.se
To: bellman@lysator.liu.se
Subject: Att fixas någon gång när du får tid

sun4 (jag kollade bara på bodil men det är väl samma...)
/usr/gnu/lib/emacs/bellmansetc kan nog tas bort. Jag fick inte.


1,,
Received: from ptolemy.arc.nasa.gov (128.102.114.134; ptolemy-ethernet.arc.nasa.gov) by lysator.liu.se (ALPHA-6.36/6.16) id AA18863; Wed, 28 Apr 1993 03:58:53 +0200 
Received: from zog.arc.nasa.gov by ptolemy.arc.nasa.gov (4.1/) id <AA00680>; Tue, 27 Apr 93 19:00:38 PDT
Date: Tue, 27 Apr 93 19:00:38 PDT
Message-Id: <9304280200.AA00680@ptolemy.arc.nasa.gov>
Received: by zog.arc.nasa.gov (4.1/SMI-4.1)
	id AA12663; Tue, 27 Apr 93 19:00:50 PDT
From: Kimball Collins <kpc@ptolemy.arc.nasa.gov>
Sender: kpc@ptolemy.arc.nasa.gov
To: linus@lysator.liu.se
Subject: virtual groups
In-Reply-To: <199304271518.AA13951@bodil.lysator.liu.se>
References: <199304271518.AA13951@bodil.lysator.liu.se>
	<9304270456.AA22169@ptolemy.arc.nasa.gov>
Reply-To: kpc@ptolemy.arc.nasa.gov

*** EOOH ***
Date: Tue, 27 Apr 93 19:00:38 PDT
From: Kimball Collins <kpc@ptolemy.arc.nasa.gov>
Sender: kpc@ptolemy.arc.nasa.gov
To: linus@lysator.liu.se
Subject: virtual groups
In-Reply-To: <199304271518.AA13951@bodil.lysator.liu.se>
References: <199304271518.AA13951@bodil.lysator.liu.se>
	<9304270456.AA22169@ptolemy.arc.nasa.gov>
Reply-To: kpc@ptolemy.arc.nasa.gov

linus@lysator.liu.se writes:
> The problem is not that the unmarked articles wont show up but that
> they are not saved anywhere.

actually my problem was that marked articles from normal groups did
not show up in a virtual group.

> These cannot be saved as numbers (as are unmarked articles in ordinary
> groups) because the numbers are generated anew everytime you enter a
> virtual group. They have to be saved as Message-IDs or possibly group
> and number.

can they be saved as ordinary group numbers using your assoc list?

--
Uniform random sigs mean "EVEN random speech is protected free speech".
otU<Sk{W`+e"OXT3}W*L03{:_;{u+\%i>IN(f-%A5*MsMDR?[_xeyH3Tl3S33<}$/YqJ{!
k)Fq(^*K`u&yIp0`&(|9Ts~Ee/%\Xr7S1_y(9ym|0bqdb~V<:;@3u3J/ysAgh}%c+KV"!W


1,, df,
Received: from quetzalcoatl.lysator.liu.se (130.236.254.133) by lysator.liu.se (ALPHA-6.36/6.16) id AA12591; Wed, 28 Apr 1993 10:44:30 +0200 
Received: by quetzalcoatl.lysator.liu.se 
          (5.51/1.34/Lysator-3.1) id AA11385; Wed, 28 Apr 93 05:59:15 +0200
          (unknown)
Message-Id: <9304280359.AA11385@quetzalcoatl.lysator.liu.se>
From: root@lysator.liu.se (Cron Daemon)
To: news@lysator.liu.se
Subject: cron for news@quetzalcoatl said this
Date: Wed Apr 28 06:00:00 1993
X-Cron-Cmd: </usr/lib/news/bin/runexpire>
X-Cron-Env: <SHELL=/bin/sh>
X-Cron-Env: <HOME=/usr/lib/news>
X-Cron-Env: <USER=news>

*** EOOH ***
From: root@lysator.liu.se (Cron Daemon)
To: news@lysator.liu.se
Subject: cron for news@quetzalcoatl said this
Date: Wed Apr 28 06:00:00 1993
X-Cron-Cmd: </usr/lib/news/bin/runexpire>
X-Cron-Env: <SHELL=/bin/sh>
X-Cron-Env: <HOME=/usr/lib/news>
X-Cron-Env: <USER=news>

Filesystem            kbytes    used   avail capacity  Mounted on
/dev/ioc/cdisk01h     433554  314762  110120    74%    /newsdisken
/dev/ioc/cdisk00f     396446  270256   86544    76%    /usr
Filesystem             iused   ifree  %iused  Mounted on
/dev/ioc/cdisk01h      95789   98771    49%   /newsdisken
/dev/ioc/cdisk00f      17289   68727    20%   /usr
startar expire     Wed Apr 28 05:59:13 MET DST 1993
startar updatemin  Wed Apr 28 10:31:45 MET DST 1993
expire klar        Wed Apr 28 10:43:51 MET DST 1993
Filesystem            kbytes    used   avail capacity  Mounted on
/dev/ioc/cdisk01h     433554  265098  159784    62%    /newsdisken
/dev/ioc/cdisk00f     396446  310162   46638    87%    /usr
Filesystem             iused   ifree  %iused  Mounted on
/dev/ioc/cdisk01h      78540  116020    40%   /newsdisken
/dev/ioc/cdisk00f      17446   68570    20%   /usr
startar nns expire


1,,
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA17563; Wed, 28 Apr 1993 12:10:05 +0200 
Date: Wed, 28 Apr 1993 12:10:05 +0200
Message-Id: <199304281010.AA17563@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: div. objekt

*** EOOH ***
Date: Wed, 28 Apr 1993 12:10:05 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Dent@lysator.liu.se (Dent i Svenskmud)
Reply-To: dent@krynn.solace.hsh.se (Dent i Svenskmud)
X-From-Mud-User: Dent i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: div. objekt

Brev från Dent i SvenskMUD.
Till:   linus

Tjiho!

[r det bara jag som irriterar mig p} alla konstiga objekt som
spelarna b{r omkring p} nu f|r tiden? shells, skal och sinnen
till exempel. Har du kollat in 'kedjeblixt' och 'snabbd|da'
i 'sinne'? Jag her bara provet 'snabbd|da' en g}ng och det
verkar vara ALLDELES f|r bra... dessutom, ska inte autoloadande
objekt INTE vara vapen etc. ?

mvh bucklan

ps. jo, jag har oxo ett 'konstigt' objekt. min buckla. men det
     {r ett rent magikerobjekt. Ingen spelare har n}gon. 
	   kolla in den f|rresten! ds.


1,,
Received: from quetzalcoatl.lysator.liu.se (130.236.254.133) by lysator.liu.se (ALPHA-6.36/6.16) id AA26377; Wed, 28 Apr 1993 14:41:36 +0200 
Received: by quetzalcoatl.lysator.liu.se 
          (5.51/1.34/Lysator-3.1) id AA20994; Wed, 28 Apr 93 14:41:34 +0200
          (unknown)
Date: Wed, 28 Apr 93 14:41:34 +0200
From: news@lysator.liu.se
Message-Id: <9304281241.AA20994@quetzalcoatl.lysator.liu.se>
Apparently-To: news@lysator.liu.se

*** EOOH ***
Date: Wed, 28 Apr 93 14:41:34 +0200
From: news@lysator.liu.se
Apparently-To: news@lysator.liu.se

rmgroup alt.cult-movies.rhps says twpierce@unix.amherst.edu (Tim Pierce) (see delgroup to do so)


1,,
Received: from lysita.lysator.liu.se (130.236.254.153) by lysator.liu.se (ALPHA-6.36/6.16) id AA10950; Wed, 28 Apr 1993 18:44:33 +0200 
Received: by lysita.lysator.liu.se 
          (ALPHA-6.36/1.34/Lysator-3.1) id AA02961; Wed, 28 Apr 1993 18:44:06 +0200
          (unknown)
Date: Wed, 28 Apr 1993 18:44:06 +0200
From: linus@lysator.liu.se
Message-Id: <199304281644.AA02961@lysita.lysator.liu.se>
To: valberedningen <thomas@lysator.liu.se>
Cc: mattias@lysator.liu.se
Subject: Fler ettor!

*** EOOH ***
Date: Wed, 28 Apr 1993 18:44:06 +0200
From: linus@lysator.liu.se
To: valberedningen <thomas@lysator.liu.se>
Cc: mattias@lysator.liu.se
Subject: Fler ettor!

Se till att f} in folk fr}n }rskurs 1 i styrelsen!

Mitt f|rslag p} kandidater till styrelsen:

Lars Sj|din, lars@lysator.liu.se
Henrik Rindl|w, rindlow@lysator.liu.se	m}ste nog |vertalas lite
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1, answered,,
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA14664; Wed, 28 Apr 1993 19:47:16 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA21914; Wed, 28 Apr 1993 19:46:57 +0200 
Date: Wed, 28 Apr 1993 19:46:57 +0200
From: mattias@lysator.liu.se
Message-Id: <199304281746.AA21914@bodil.lysator.liu.se>
To: linus@lysator.liu.se
Cc: mattias@lysator.liu.se, thomas@lysator.liu.se
In-Reply-To: linus@lysator.liu.se's message of Wed, 28 Apr 1993 18:44:06 +0200 <199304281644.AA02961@lysita.lysator.liu.se>
Subject: Fler ettor!

*** EOOH ***
Date: Wed, 28 Apr 1993 19:46:57 +0200
From: mattias@lysator.liu.se
To: linus@lysator.liu.se
Cc: mattias@lysator.liu.se, thomas@lysator.liu.se
In-Reply-To: linus@lysator.liu.se's message of Wed, 28 Apr 1993 18:44:06 +0200 <199304281644.AA02961@lysita.lysator.liu.se>
Subject: Fler ettor!

"m}ste nog |vertalas lite" gillar jag inte. Vi vill ha en styrelse som
_vill_ vara med i styrelsen, inte som s{ger "ok d}...".

Vilka {r dom f|rresten?
---
A religion dies when it is proven true. Science |       Mattias Olofsson |
is the record of dead religions.    Oscar Wilde | mattias@lysator.liu.se |


1,,
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA15882; Wed, 28 Apr 1993 20:07:51 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA21966; Wed, 28 Apr 1993 20:07:32 +0200 
Date: Wed, 28 Apr 1993 20:07:32 +0200
From: linus@lysator.liu.se
Message-Id: <199304281807.AA21966@bodil.lysator.liu.se>
To: mattias@lysator.liu.se
Cc: mattias@lysator.liu.se, thomas@lysator.liu.se
In-Reply-To: mattias@lysator.liu.se's message of Wed, 28 Apr 1993 19:46:57 +0200 <199304281746.AA21914@bodil.lysator.liu.se>
Subject: Fler ettor!

*** EOOH ***
Date: Wed, 28 Apr 1993 20:07:32 +0200
From: linus@lysator.liu.se
To: mattias@lysator.liu.se
Cc: mattias@lysator.liu.se, thomas@lysator.liu.se
In-Reply-To: mattias@lysator.liu.se's message of Wed, 28 Apr 1993 19:46:57 +0200 <199304281746.AA21914@bodil.lysator.liu.se>
Subject: Fler ettor!

De g}r i D1d. Sitter mest inne p} ida men tittar ut n}gon g}ng om
dygnet. De skriver raytracer och {r magiker i BSX-mud!

M}ste nog |vertalas lite beror p} att jag inte har pratat med honom
och att Lars sade att han nog har saker att g|ra inom sektionen. Det
kan ni v{l kolla n{r ni fr}gar.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Received: from bodil.lysator.liu.se (130.236.254.152) by lysator.liu.se (ALPHA-6.36/6.16) id AA16477; Wed, 28 Apr 1993 20:17:52 +0200 
Received: by bodil.lysator.liu.se (ALPHA-6.36/6.16) id AA22039; Wed, 28 Apr 1993 20:17:33 +0200 
Date: Wed, 28 Apr 1993 20:17:33 +0200
From: mattias@lysator.liu.se
Message-Id: <199304281817.AA22039@bodil.lysator.liu.se>
To: linus@lysator.liu.se
Cc: thomas@lysator.liu.se
In-Reply-To: linus@lysator.liu.se's message of Wed, 28 Apr 1993 20:07:32 +0200 <199304281807.AA21966@bodil.lysator.liu.se>
Subject: Fler ettor!

*** EOOH ***
Date: Wed, 28 Apr 1993 20:17:33 +0200
From: mattias@lysator.liu.se
To: linus@lysator.liu.se
Cc: thomas@lysator.liu.se
In-Reply-To: linus@lysator.liu.se's message of Wed, 28 Apr 1993 20:07:32 +0200 <199304281807.AA21966@bodil.lysator.liu.se>
Subject: Fler ettor!

Fast vi har inte s} m}nga platser kvar att fylla. 1 eller 1&1/2 i sty,
1 revisor och 2 valberedning (vilket _inte_ ska vara ettor.)
---
A religion dies when it is proven true. Science |       Mattias Olofsson |
is the record of dead religions.    Oscar Wilde | mattias@lysator.liu.se |


1,,
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA19520; Wed, 28 Apr 1993 21:10:05 +0200 
Date: Wed, 28 Apr 1993 21:10:05 +0200
Message-Id: <199304281910.AA19520@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Carlsberg@lysator.liu.se (Carlsberg i Svenskmud)
Reply-To: td92mhn@te.hik.se (Carlsberg i Svenskmud)
X-From-Mud-User: Carlsberg i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: bugg under strid

*** EOOH ***
Date: Wed, 28 Apr 1993 21:10:05 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Carlsberg@lysator.liu.se (Carlsberg i Svenskmud)
Reply-To: td92mhn@te.hik.se (Carlsberg i Svenskmud)
X-From-Mud-User: Carlsberg i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: bugg under strid

Brev från Carlsberg i SvenskMUD.
Till:   linus

Under strid med m|rkermagiker i skaven brunnen f|rlorade
jag erfarenhetspo{ng och lita av min utrustning...
jag f|rstod inte riktigt vad som h{nde men mystiskt
var det..
Jag fick hj{lp av Edvard att st{lla allt till r{tta...Du kan kontakta
honom eller Hades om du har n}gra fr}gor...
Du f}r urs{kta det h{r kryptiska brevet men som sagt f|rst}r
jag inte vad som h{nt...
Ha en bra dag
Mvh \l (klassIII)


1,,
Received: from skinner.cs.uoregon.edu (128.223.4.13) by lysator.liu.se (ALPHA-6.36/6.16) id AA16837; Thu, 29 Apr 1993 05:13:53 +0200 
Received: from efn.UUCP by skinner.cs.uoregon.edu with UUCP id AA11498
  (5.65/IDA-1.4.2 for bug-lyskom@lysator.liu.se); Wed, 28 Apr 93 20:13:40 -0700
Received: by efn (4.1/smail2.5/05-07-92)
	id AA12815; Wed, 28 Apr 93 20:07:11 PDT
Date: Wed, 28 Apr 93 20:07:11 PDT
From: clif@efn.org (Clif Cox)
Message-Id: <9304290307.AA12815@efn>
To: bug-lyskom@lysator.liu.se
Subject: Need help with KOM

*** EOOH ***
Date: Wed, 28 Apr 93 20:07:11 PDT
From: clif@efn.org (Clif Cox)
To: bug-lyskom@lysator.liu.se
Subject: Need help with KOM


Hi I'm with a group of people in Eugene Or. that are putting together a 
community network modeled losely after community memory in the Bay Area, and
the Clevland free net.

A friend has highly recamended KOM as an excellent connferencing system.
We will initially have 16 phone lines, and hope to grow to over 50, Do you
think somthing like a SPARC II running KOM would handle that (implied) load?

As the head of the Tech committy I would realy like to evalulate KOM to give
it a chance aganst the current favorate of most of the other board  members
which is Caucus by Camber-Roth.  I don't yet have much experiance with it but
in general I'm not too impressed.  I would like to support a client server model
for our system, so I am excited that KOM is designed with this in mind.

I've been working to build KOM  1.2.5, is that the current version?  Right now
I'm finding I don't have crypt.h or authuser.h on my Sun3 running SunOs4.1.1.
Are those in an optional instalation?  Can I reconfigure to get by without them
for now?

	Thanks for your help,

	Clif Cox


1,,
Received: from godot.lysator.liu.se (130.236.254.154) by lysator.liu.se (ALPHA-6.36/6.16) id AA07100; Thu, 29 Apr 1993 11:13:44 +0200 
Received: by godot.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA29883; Thu, 29 Apr 93 11:13:36 +0200
          (unknown)
Date: Thu, 29 Apr 93 11:13:33 MET DST
From: Peter Eriksson <pen@lysator.liu.se>
To: lysator@lysator.liu.se
Subject: [stawi@ppvlu.ericsson.se (Staffan Wiklund): Fvrfregan om medlemsskap
        i Lysator]
Message-Id: <CMM.0.90.0.736074813.pen@godot.lysator.liu.se>

*** EOOH ***
Date: Thu, 29 Apr 93 11:13:33 MET DST
From: Peter Eriksson <pen@lysator.liu.se>
To: lysator@lysator.liu.se
Subject: [stawi@ppvlu.ericsson.se (Staffan Wiklund): Fvrfregan om medlemsskap
        i Lysator]

Received: from mailgate.ericsson.se (130.100.2.2) by lysator.liu.se (ALPHA-6.36/6.16) id AA27116; Thu, 29 Apr 1993 08:22:26 +0200 
Received: from ppvku.ericsson.se by mailgate.ericsson.se (4.1/SMI-4.1-MAILGATE1.13)
	id AA11670; Thu, 29 Apr 93 07:21:34 +0200
Received: from bach.ericsson.se (ppvlu) by ppvku.ericsson.se (4.1/SMI-4.1)
	id AA22892; Thu, 29 Apr 93 07:23:57 +0200
Received: from ppls3.ericsson.se by bach.ericsson.se (4.1/SMI-4.1)
	id AA02833; Thu, 29 Apr 93 07:19:30 +0200
Date: Thu, 29 Apr 93 07:19:30 +0200
From: stawi@ppvlu.ericsson.se (Staffan Wiklund)
Message-Id: <9304290519.AA02833@bach.ericsson.se>
To: postmaster@lysator.liu.se
Subject: Förfrågan om medlemsskap i Lysator
Cc: stawi@ppvlu.ericsson.se

Hej

Jag heter Staffan Wiklund och arbetar på Programatic AB i Mjärdevi.

Jag undrar om det är möjligt för mig att ansöka om medlemsskap i Lysator.
Jag studerar inte vid LiTH längre, men jag har gått D-linjen.

Syftet är endast av hobbykaraktär dvs. ett allmänt dataintresse.



Mvh

Staffan Wiklund     tel. 20 48 12

Programatic
Datalinjen 3
583 30 Linköping


Peter Eriksson                                            pen@lysator.liu.se
Lysator Academic Computer Society               ...!uunet!lysator.liu.se!pen
University of Linkoping, Sweden                           <Something boring>


1,,
Received: from godot.lysator.liu.se (130.236.254.154) by lysator.liu.se (ALPHA-6.36/6.16) id AA07412; Thu, 29 Apr 1993 11:19:39 +0200 
Received: by godot.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA29905; Thu, 29 Apr 93 11:19:31 +0200
          (unknown)
Date: Thu, 29 Apr 93 11:19:31 +0200
From: ceder@lysator.liu.se
Message-Id: <9304290919.AA29905@godot.lysator.liu.se>
To: clif@efn.org
Cc: bug-lyskom@lysator.liu.se
In-Reply-To: Clif Cox's message of Wed, 28 Apr 93 20:07:11 PDT <9304290307.AA12815@efn>
Subject: Need help with KOM

*** EOOH ***
Date: Thu, 29 Apr 93 11:19:31 +0200
From: ceder@lysator.liu.se
To: clif@efn.org
Cc: bug-lyskom@lysator.liu.se
In-Reply-To: Clif Cox's message of Wed, 28 Apr 93 20:07:11 PDT <9304290307.AA12815@efn>
Subject: Need help with KOM

> A friend has highly recamended KOM as an excellent connferencing system.
> We will initially have 16 phone lines, and hope to grow to over 50, Do you
> think somthing like a SPARC II running KOM would handle that (implied) load?

Well... The server could handle the load without problem, but if you
are going to run 16 Emacs client on the same machine it will probably
be rather slow. The Emacs client (lyskom.el) takes a lot of resources.

If you are going to run SLIP/PPP over the telephone lines, and the KOM
clients on the machine that connects to the server, everything should
be fine.  We are currently running the server on a Sun-3.

We are writing a couple of new clients in C and C++, and when they are
ready they could handle the load. The client in C is available from
ftp.lysator.liu.se in pub/lyskom/tty-client. Version L0.09 is the
latest. It is beginning to be stable, but it typically dumps core a
couple of times per day. The C++-client will probably not be useable
until july/august, at the earliest.

> I've been working to build KOM 1.2.5, is that the current version?
> Right now I'm finding I don't have crypt.h or authuser.h on my Sun3
> running SunOs4.1.1.  Are those in an optional instalation?  Can I
> reconfigure to get by without them for now?

That is the latest version available via ftp. We are running 1.3.3.
The changes are not big. Mostly minor cleanup, and experimental things
that are anyhow unused by the current clients.

You can get the authuser package from ftp.lysator.liu.se. It contains
authuser.h. It is in pub/ident/libs/libauth-4.0-p4.tar.Z.

I also had a hard time finding crypt.h on our system. Finally I found
it in a local include dir we have, where we store some protoized
include headers. Some day I'm going to fix the installation of the
LysKOM server. It depends too much on local features we have here at
Lysator. Anyway, this is how crypt.h looks:

char *crypt(const char *key, const char *salt);
char *_crypt(char *key, char *salt);
void setkey(char *key);
void encrypt(char *block, int edflag);

(Yes, only four lines). You can probably do without it, but you will
get a warning from gcc...

Don't hesitate to send a new mail if you have any other problems.

	Per Cederqvist


1,,
Received: from varg.lysator.liu.se (130.236.254.151) by lysator.liu.se (ALPHA-6.36/6.16) id AA08636; Thu, 29 Apr 1993 11:41:16 +0200 
Received: by varg.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA08550; Thu, 29 Apr 93 11:40:41 +0200
          (unknown)
Date: Thu, 29 Apr 93 11:40:41 +0200
From: svmud@lysator.liu.se
Message-Id: <9304290940.AA08550@varg.lysator.liu.se>
To: svmud@lysator.liu.se
Subject: SvenskMUDs omstart nummer 15 gick ner. Exit status: 2

*** EOOH ***
Date: Thu, 29 Apr 93 11:40:41 +0200
From: svmud@lysator.liu.se
To: svmud@lysator.liu.se
Subject: SvenskMUDs omstart nummer 15 gick ner. Exit status: 2

'       ge_knark' in 'spelare/mikael/larling.c' ('spelare/mikael/larling#23953')line 222
Totally out of MEMORY.
'           go_n' in '        obj/player.c' ('    obj/player#24267')line 3616
'       try_move' in '        obj/player.c' ('    obj/player#24267')line 3609
'        command' in '        std/object.c' ('    obj/player#24267')line 91
'           move' in '           rum/rum.c' ('rum/strandhamn/bygata1')line 137
'    move_player' in '        obj/living.c' ('    obj/player#24267')line 189
'    move_object' in '        std/object.c' ('    obj/player#24267')line 83
Exit status:  2
Thu Apr 22 14:37:22 1993 Game shutdown by kiwi(en massa buggar!)
Bad argument 1 to call_other()
program: spelare/mikael/larling.c, object: spelare/mikael/larling#23953 line 222
'       ge_knark' in 'spelare/mikael/larling.c' ('spelare/mikael/larling#23953')line 222
Error in call out.
'           go_n' in '        obj/player.c' ('    obj/player#24267')line 3616
'       try_move' in '        obj/player.c' ('    obj/player#24267')line 3609
'        command' in '        std/object.c' ('    obj/player#24267')line 91
'           move' in '           rum/rum.c' ('rum/strandhamn/bygata1')line 137
'    move_player' in '        obj/living.c' ('    obj/player#24267')line 189
'    move_object' in '        std/object.c' ('    obj/player#24267')line 83
pen      ttyp1   Apr 29 11:09	(nanny.lysator.li)
       F UID   PID  PPID CP PRI NI  SZ  RSS WCHAN        STAT TT  TIME COMMAND
   80003   0     0     0  0 -25  0   0    0 runout       D    ?  11:59 swapper
20088001   0     1     0  0   5  0  56   40 child        S    ?   0:13 /sbin/init -
   80003   0     2     0  0 -24  0   0    0 child        D    ?  64:06 pagedaemon
   88000   0    55     1  0   1  0  80    0 select       IW   ?   2:06 portmap
   88000   3    58     1  0   1  0  40    0 select       IW   ?   0:00 ypbind
   88000   0    60     1  0   1  0  56    0 select       IW   ?   0:01 keyserv
   88001   0    73     1  1   1  0  24    0 nfs_dnlc     S    ?   4:15  (biod)
   88001   0    74     1  0   1  0  24    0 nfs_dnlc     S    ?   4:10  (biod)
   88001   0    75     1  0   1  0  24    0 nfs_dnlc     S    ?   4:17  (biod)
   88001   0    76     1  0   1  0  24    0 nfs_dnlc     S    ?   4:16  (biod)
   88000   0    86     1  0   1  0  64    0 select       IW   ?   0:28 syslogd
  488001   0    98     1  0   1  0  40    0 socket       S    ?  25:12  (nfsd)
   88000   0    99     1  1   1  0  72    0 select       IW   ?   3:51 rpc.mountd -n
   88001   0   102    98  0   1  0  40    0 socket       S    ?  25:01  (nfsd)
   88001   0   103    98  0   1  0  40    0 socket       S    ?  25:29  (nfsd)
   88001   0   104    98  0   1  0  40    0 socket       S    ?  25:21  (nfsd)
   88001   0   105    98  0   1  0  40    0 socket       S    ?  25:11  (nfsd)
   88001   0   107    98  0   1  0  40    0 socket       S    ?  25:33  (nfsd)
   88001   0   108    98  0   1  0  40    0 socket       S    ?  25:08  (nfsd)
   88001   0   109    98  0   1  0  40    0 socket       S    ?  25:22  (nfsd)
   88000   0   110     1  1   1  0 144    0 select       IW   ?   0:05 rpc.lockd
   88000   0   111     1  0   1  0  72    0 select       IW   ?   0:00 rpc.statd
   80201   0   127     1  0  15  0  24   16 kernelmap    S    ? 100:25 update
  488000   0   130     1  1   1  0  64    0 usrpt        IW   ?   0:49 cron
  488001   0   136     1  0   1  0  48   80 socket       S    ?  28:35 in.rwhod
   88000   0   141     1  0   1  0  56    0 select       IW   ?   0:00 /usr/lib/lpd
   88000   0   770     1  0   1  0 224    0 select       IW   ?   1:04 /usr/X11/bin/xdm
20088000   0  1614 11892  0   1  0  64    0 select       IW   ?   0:00 in.rshd
20488401 124  1615  1614 32  33  0   0    0              Z    ?   0:00 <defunct>
20080000 124  1663     1  6   1  0  56    0 select       IW   ?   0:00 rsh -n robert /usr/X11/bin/xterm -geometry 110x63+0-2 -n irc -j -e /usr/local/bin/irc
   80401 124  1665  1663  1  32  0   0    0              Z    ?   0:00 <defunct>
200880014893  4814     1  3   1  012776 2728 select       S    ? 549:06 /home/lyskom/bin/lyskomd -p4894 -P4895 -D/home/lyskom
20088000   0  8517 11892  0   1  0  40    0 select       IW   ?   0:00 in.rlogind
20080001  21  8545 23311 15  28  0 224  520              R    ?   0:00 ps alxww
20088001  21  8546 23311  3   1  0  72  376 socket       S    ?   0:00 Mail -s SvenskMUDs omstart nummer 15 gick ner. Exit status: 2 svmud
   88000   0 11892     1  0   1  0  56    0 select       IW   ?   6:23 inetd
20088201  21 23311     1  2  15  0  72  184 kernelmap    S    ?   0:03 /usr/bin/csh -b bin/restartmud
   88001   0 24513     1  2   1  0 128  240 select       S    ?  10:48 automount
80088000 259 24649     1  0   1  0  88    0 select       IW   ?  54:38 lpmud/bin/mudfingerd -l
20088000   0  4720     1  4   3  0  56    0 usrpt        IW   co  0:00 - std.9600 console (getty)
20488000 121  8518  8517  0   3  0 224    0 usrpt        IW   p1  0:03 -tcsh (tcsh)


1,,
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA01798; Thu, 29 Apr 1993 18:10:03 +0200 
Date: Thu, 29 Apr 1993 18:10:03 +0200
Message-Id: <199304291610.AA01798@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: V{rsting@lysator.liu.se (V{rsting i Svenskmud)
Reply-To: tet92tg@te.hik.se (V{rsting i Svenskmud)
X-From-Mud-User: V{rsting i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Edvards hundar

*** EOOH ***
Date: Thu, 29 Apr 1993 18:10:03 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: V{rsting@lysator.liu.se (V{rsting i Svenskmud)
Reply-To: tet92tg@te.hik.se (V{rsting i Svenskmud)
X-From-Mud-User: V{rsting i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Edvards hundar

Brev från V{rsting i SvenskMUD.
Till:   linus

Jag vet inte om du k{nner till Edvards nya hundar, men det har visat
sig att dem {r alldeles f|r bra f|r spelarna. Jag har ett exempel p} en
spelare p} niv} 18, som d|dar Tor och Oden p} 1 g}ng utan att f|rlora
annat {n magipo{ng. Det {r F\R bra. Har {ven pratat med andra magiker om
detta och de h}ller med mig. Det {r ju en kul ide, men han f}r nog s{nka
niv}n lite p} hundarna, om det ska vara bra. Har f|r |vrigt inte alls
n}got emot Edvard och hans ideer.
              M.V.H V{rsting


1,,
Received: from quetzalcoatl.lysator.liu.se (130.236.254.133) by lysator.liu.se (ALPHA-6.36/6.16) id AA16490; Thu, 29 Apr 1993 22:33:32 +0200 
Received: by quetzalcoatl.lysator.liu.se 
          (5.51/1.34/Lysator-3.1) id AA00382; Thu, 29 Apr 93 22:33:33 +0200
          (unknown)
Date: Thu, 29 Apr 93 22:33:33 +0200
From: news@lysator.liu.se
Message-Id: <9304292033.AA00382@quetzalcoatl.lysator.liu.se>
Subject: possible active file problems
Apparently-To: news@lysator.liu.se

*** EOOH ***
Date: Thu, 29 Apr 93 22:33:33 +0200
From: news@lysator.liu.se
Subject: possible active file problems
Apparently-To: news@lysator.liu.se

If you believe this checkgroups control message, the following
differences may reflect groups that should be added, deleted,
or have their moderation status(es) changed:

obsolete groups:
eunet.euug
eunet.followup
eunet.general
eunet.micro.acorn


1,,
Received: from quetzalcoatl.lysator.liu.se (130.236.254.133) by lysator.liu.se (ALPHA-6.36/6.16) id AA16594; Thu, 29 Apr 1993 22:35:31 +0200 
Received: by quetzalcoatl.lysator.liu.se 
          (5.51/1.34/Lysator-3.1) id AA00472; Thu, 29 Apr 93 22:35:26 +0200
          (unknown)
Date: Thu, 29 Apr 93 22:35:26 +0200
From: news@lysator.liu.se
Message-Id: <9304292035.AA00472@quetzalcoatl.lysator.liu.se>
Subject: possible active file problems
Apparently-To: news@lysator.liu.se

*** EOOH ***
Date: Thu, 29 Apr 93 22:35:26 +0200
From: news@lysator.liu.se
Subject: possible active file problems
Apparently-To: news@lysator.liu.se

If you believe this checkgroups control message, the following
differences may reflect groups that should be added, deleted,
or have their moderation status(es) changed:

obsolete groups:
alt.2d
alt.abuse-recovery
alt.abuse.offender.recovery
alt.abuse.recovery
alt.adjective.noun.verb.verb.verb
alt.agriculture.misc
alt.aldus.freehand
alt.aldus.pagemaker
alt.alien.vampire.flonk.flonk.flonk
alt.anonymous
alt.answers
alt.autos.antique
alt.autos.karting
alt.bad.clams
alt.baldspot
alt.barney.dinosaur.die.die.die
alt.bbs.doors
alt.bbs.uupcb
alt.bbs.waffle
alt.best.of.internet
alt.bigfoot
alt.bitch.pork
alt.bitterness
alt.bob-packwood.tongue.tongue.tongue
alt.books.reviews
alt.buddha.short.fat.guy
alt.business.misc
alt.butt.harp
alt.cabal
alt.clubs.compsci
alt.comics.lnh
alt.comics.superman
alt.comp.compression
alt.cows.moo.moo.moo
alt.cult-movies.rhps
alt.cult-movies.rocky-horror
alt.culture.ny.upstate
alt.culture.tamil
alt.current-events.blizzard-of-93
alt.current-events.inet92
alt.current-events.wtc-explosion
alt.cybertoon
alt.dear.whitehouse
alt.delete.this.newsgroup
alt.depew.aarm.aarm.aarm.aarm.aarm.aarm.aarm.aarm
alt.dev.null
alt.devilbunnies
alt.dragons-inn
alt.drugs.usenet
alt.elvis.sighting
alt.ensign.wesley.die.die.die
alt.ernie-pook
alt.extropians
alt.extropians.forbidden.topics
alt.fan.TinyTIM
alt.fan.amy-fisher
alt.fan.don.no-soul.simmons
alt.fan.gene-scott
alt.fan.gooley
alt.fan.goons
alt.fan.ken-johnson
alt.fan.lemurs.cooked
alt.fan.matt.welsh
alt.fan.mst3k
alt.fan.oingo-boingo
alt.fan.poris
alt.fan.robert.mcelwaine
alt.fan.tank-girl
alt.fan.tim-pierce.control.control.control
alt.fan.wang-chung
alt.fan.warren.burstein
alt.fan.woody-allen
alt.fax
alt.flame.hannigan
alt.flame.marshal.perlman.weenie
alt.flame.sean-ryan
alt.flame.spelling
alt.fondle.vomit
alt.food.cocacola
alt.food.dennys
alt.food.mcdonalds
alt.foolish.users
alt.french.captain.borg.borg.borg
alt.gambling
alt.games.mornington.crescent
alt.games.omega
alt.gathering.rainbow
alt.geek
alt.genealogy
alt.good.morning
alt.graffiti
alt.graphics
alt.guinea.pig.conspiracy
alt.happy.birthday.to.me
alt.hinz.und.grunz
alt.history.living
alt.horror.shub-internet
alt.housing.nontrad
alt.internet.talk.radio
alt.irc.corruption
alt.is.too
alt.isea
alt.journalism.music
alt.kalbo
alt.ketchup
alt.kodak.cd.bitch.bitch.bitch
alt.lang.awk
alt.lwaxana-troi.die.die.die
alt.machines.misc
alt.magnus.and.ketil
alt.mcdonalds.cheese
alt.mcdonalds.food
alt.mcdonalds.ketchup
alt.mcdonalds.smut
alt.mcdonalds.vegemite
alt.mindcontrol
alt.motd
alt.motorcycles.harley
alt.mud.bsx
alt.mud.cyberworld
alt.mud.t-rev.stomp.stomp.stomp
alt.music.category-freak
alt.music.enya.puke.puke.puke
alt.music.journalism
alt.music.karaoke
alt.music.machines.of.loving.grace
alt.music.pop.will.eat.itself
alt.music.pop.will.eat.itself.X.Y.and.Z.electrix.sunshine.mix
alt.music.pop.will.eat.itself.the.poppies.are.on.patrol
alt.music.the.police
alt.music.the.police.ctl
alt.my.crummy.boss
alt.my.head.hurts
alt.necktie
alt.net.personalities
alt.newbie
alt.newbies
alt.newgroup
alt.nick.sucks
alt.noise
alt.non.sequitur
alt.olympics.medal-tally
alt.oobe
alt.org.food-not-bombs
alt.os.bsdi
alt.os.linux
alt.os.nachos
alt.parents.analretentive.insane
alt.pave.the.earth
alt.peace-corps
alt.peace.corps
alt.periphs.pcmcia
alt.pets.chia
alt.pictures.fuzzy.animals
alt.politics.british
alt.politics.bush
alt.politics.democrats
alt.politics.democrats.clinton
alt.politics.democrats.d
alt.politics.democrats.governors
alt.politics.democrats.house
alt.politics.democrats.senate
alt.politics.economics
alt.politics.equality
alt.politics.italy
alt.politics.kibo
alt.politics.marrou
alt.politics.media
alt.politics.shelfbutt
alt.ql.creative
alt.radio.internet
alt.recovery.codependency
alt.religion.adm3a
alt.religion.santaism
alt.religion.vince
alt.rissa
alt.rodney.dangerfield
alt.rodney.king
alt.sadistic.dentists.drill.drill.drill
alt.sca
alt.sci.image-facility
alt.security.keydist
alt.sex.NOT
alt.sex.aluminum.baseball.bat
alt.sex.bestiality.hamster.duct-tape
alt.sex.bondage.particle.physics
alt.sex.boredom
alt.sex.carasso
alt.sex.carasso.snuggles
alt.sex.fetish.feet
alt.sex.graphics
alt.sex.homosexual
alt.sex.nudels.me.too
alt.sex.pictures
alt.sex.pictures.d
alt.sex.sonja
alt.sex.sounds
alt.sex.trans
alt.sex.woody-allen
alt.sexual.abuse.recovery.d
alt.sexy.bald.captains
alt.sf4m
alt.showbiz.gossip
alt.shrinky.dinks
alt.shut.the.hell.up.geek
alt.sigma2.penis
alt.silly.group.names.d
alt.slick.willy.tax.tax.tax
alt.smouldering.dog.zone
alt.society.cu-digest
alt.source-code
alt.sources.amiga.d
alt.spam
alt.spam.tin
alt.spleen
alt.sport.photon
alt.sport.pool
alt.stupid.putz
alt.suicide.finals
alt.superman.dead
alt.support.mult-sclerosis
alt.swedish.chef.bork.bork.bork
alt.sys.pc532
alt.sys.unisys
alt.tasteless.penis
alt.tasteless.pictures
alt.tennis
alt.test.my.new.group
alt.thinking.hurts
alt.timewasters
alt.true.crime
alt.tv.90210
alt.tv.dinosaur.barney.die.die.die
alt.tv.dinosaurs.barney.die.die.die
alt.tv.fifteen
alt.tv.infomercials
alt.tv.mwc
alt.tv.saved-bell
alt.tv.tiny-toon.plucky-duck
alt.unix.wizards
alt.vigilantes
alt.wall
alt.wanted.mars.women
alt.wanted.moslem.gay
alt.wanted.moslem.men
alt.wanted.moslem.women
alt.waves
alt.weemba
alt.wesley-dodd.hang.hang.hang
alt.who.is.bob
alt.windows.text
bionet.biology.n2-fixation
bionet.molbio.gene-org
bionet.molbio.news
bionet.molbio.pir
bionet.molbio.swiss-prot
bionet.photosynthesis
bionet.software.contrib
bionet.software.pc
bionet.software.pc.comm
bionet.technology.conversion
comp.dcom.lans
comp.mail.maps
comp.org.uniforum
comp.os.os2
comp.sys.amiga
comp.sys.amiga.tech
comp.sys.ibm.hardware
comp.sys.ibm.pc
comp.sys.ibm.pc.programmer
comp.sys.mac
comp.sys.sgi
comp.unix.i386
comp.unix.msdos
comp.unix.sysv386
comp.windows.ms
comp.windows.ms.programmer
gnu.config
gnu.emacs
gnu.emacs.lisp.manual
gnu.g++
gnu.gcc
gnu.test
misc.forsale.computers
misc.forsale.wanted
news.admin
news.newsites
rec.arts.comics
rec.arts.sf-lovers
rec.arts.sf-reviews
rec.arts.startrek
rec.autos.subaru
rec.aviation
rec.bicycles
rec.games.frp
rec.games.mud
rec.games.mud.wine.bitch.moan
rec.ham-radio
rec.ham-radio.packet
rec.ham-radio.swap
rec.music.synth
rec.org.mensa.flame.flame.flame
rec.sport.basketball
rec.sport.football
sci.chaos

new groups:
alt.binaries.multimedia
alt.binaries.pictures.d
alt.binaries.pictures.erotica
alt.binaries.pictures.erotica.blondes
alt.binaries.pictures.erotica.d
alt.binaries.pictures.erotica.female
alt.binaries.pictures.erotica.male
alt.binaries.pictures.fine-art.d
alt.binaries.pictures.fine-art.digitized
alt.binaries.pictures.fine-art.graphics
alt.binaries.pictures.fractals
alt.binaries.pictures.misc
alt.binaries.pictures.supermodels
alt.binaries.pictures.tasteless
alt.binaries.pictures.utilities
alt.binaries.sounds.d
alt.binaries.sounds.misc
alt.cable-tv.re-regulate
alt.cesium
alt.decathena
alt.fan.nathan.brazil
alt.fan.ren-and-stimpy
alt.fan.wal-greenslade
alt.fractals.pictures
alt.homosexual
alt.med.cfs
alt.society.foia
alt.true-crime
alt.tv.melrose-place
alt.tv.simpsons.itchy-scratchy
alt.znet.pc
bit.admin
bit.general
bit.listserv.3com-l
bit.listserv.9370-l
bit.listserv.ada-law
bit.listserv.advanc-l
bit.listserv.advise-l
bit.listserv.aix-l
bit.listserv.allmusic
bit.listserv.appc-l
bit.listserv.apple2-l
bit.listserv.applicat
bit.listserv.arie-l
bit.listserv.ashe-l
bit.listserv.asm370
bit.listserv.autism
bit.listserv.banyan-l
bit.listserv.big-lan
bit.listserv.billing
bit.listserv.biosph-l
bit.listserv.bitnews
bit.listserv.blindnws
bit.listserv.buslib-l
bit.listserv.c+health
bit.listserv.c18-l
bit.listserv.c370-l
bit.listserv.candle-l
bit.listserv.catholic
bit.listserv.cdromlan
bit.listserv.cfs.newsletter
bit.listserv.christia
bit.listserv.cics-l
bit.listserv.cinema-l
bit.listserv.circplus
bit.listserv.cmspip-l
bit.listserv.commed
bit.listserv.csg-l
bit.listserv.cumrec-l
bit.listserv.cw-email
bit.listserv.cwis-l
bit.listserv.cyber-l
bit.listserv.dasig
bit.listserv.db2-l
bit.listserv.dbase-l
bit.listserv.deaf-l
bit.listserv.decnews
bit.listserv.dectei-l
bit.listserv.devel-l
bit.listserv.disarm-l
bit.listserv.domain-l
bit.listserv.earntech
bit.listserv.ecolog-l
bit.listserv.edi-l
bit.listserv.edpolyan
bit.listserv.edstat-l
bit.listserv.edtech
bit.listserv.edusig-l
bit.listserv.emusic-l
bit.listserv.endnote
bit.listserv.envbeh-l
bit.listserv.erl-l
bit.listserv.ethics-l
bit.listserv.ethology
bit.listserv.euearn-l
bit.listserv.film-l
bit.listserv.fnord-l
bit.listserv.frac-l
bit.listserv.free-l
bit.listserv.games-l
bit.listserv.gaynet
bit.listserv.gddm-l
bit.listserv.geodesic
bit.listserv.gguide
bit.listserv.govdoc-l
bit.listserv.gutnberg
bit.listserv.hellas
bit.listserv.help-net
bit.listserv.hindu-d
bit.listserv.history
bit.listserv.hytel-l
bit.listserv.i-amiga
bit.listserv.ibm-hesc
bit.listserv.ibm-main
bit.listserv.ibm-nets
bit.listserv.ibm7171
bit.listserv.ibmtcp-l
bit.listserv.india-d
bit.listserv.info-gcg
bit.listserv.ingrafx
bit.listserv.innopac
bit.listserv.ioob-l
bit.listserv.ipct-l
bit.listserv.isn
bit.listserv.jes2-l
bit.listserv.jnet-l
bit.listserv.l-hcap
bit.listserv.l-vmctr
bit.listserv.lawsch-l
bit.listserv.liaison
bit.listserv.libref-l
bit.listserv.libres
bit.listserv.license
bit.listserv.linkfail
bit.listserv.literary
bit.listserv.lstsrv-l
bit.listserv.mail-l
bit.listserv.mailbook
bit.listserv.mba-l
bit.listserv.mbu-l
bit.listserv.mdphd-l
bit.listserv.medforum
bit.listserv.medlib-l
bit.listserv.mednews
bit.listserv.mideur-l
bit.listserv.netnws-l
bit.listserv.nettrain
bit.listserv.new-list
bit.listserv.next-l
bit.listserv.nodmgt-l
bit.listserv.notabene
bit.listserv.notis-l
bit.listserv.novell
bit.listserv.omrscan
bit.listserv.os2-l
bit.listserv.ozone
bit.listserv.pacs-l
bit.listserv.page-l
bit.listserv.pagemakr
bit.listserv.pmdf-l
bit.listserv.politics
bit.listserv.postcard
bit.listserv.power-l
bit.listserv.powerh-l
bit.listserv.psycgrad
bit.listserv.qualrs-l
bit.listserv.relusr-l
bit.listserv.rhetoric
bit.listserv.rscs-l
bit.listserv.rscsmods
bit.listserv.s-comput
bit.listserv.sas-l
bit.listserv.script-l
bit.listserv.scuba-l
bit.listserv.seasia-l
bit.listserv.seds-l
bit.listserv.sfs-l
bit.listserv.sganet
bit.listserv.simula
bit.listserv.slart-l
bit.listserv.slovak-l
bit.listserv.snamgt-l
bit.listserv.sos-data
bit.listserv.spires-l
bit.listserv.sportpsy
bit.listserv.spssx-l
bit.listserv.sqlinfo
bit.listserv.stat-l
bit.listserv.tech-l
bit.listserv.techwr-l
bit.listserv.tesl-l
bit.listserv.test
bit.listserv.tex-l
bit.listserv.tn3270-l
bit.listserv.toolb-l
bit.listserv.trans-l
bit.listserv.travel-l
bit.listserv.ucp-l
bit.listserv.ug-l
bit.listserv.uigis-l
bit.listserv.urep-l
bit.listserv.usrdir-l
bit.listserv.uus-l
bit.listserv.valert-l
bit.listserv.vfort-l
bit.listserv.vm-util
bit.listserv.vmesa-l
bit.listserv.vmslsv-l
bit.listserv.vmxa-l
bit.listserv.vnews-l
bit.listserv.vpiej-l
bit.listserv.win3-l
bit.listserv.words-l
bit.listserv.wpcorp-l
bit.listserv.wpwin-l
bit.listserv.x400-l
bit.listserv.xcult-l
bit.listserv.xedit-l
bit.listserv.xerox-l
bit.listserv.xmailer
bit.listserv.xtropy-l
bit.mailserv.word-mac
bit.mailserv.word-pc
biz.americast
biz.americast.samples
biz.dec.decathena
biz.dec.decnews
biz.dec.xmedia
biz.digex.announce
biz.sco.binaries
biz.zeos.general
comp.binaries.acorn
comp.binaries.amiga
comp.binaries.apple2
comp.binaries.atari.st
comp.binaries.ibm.pc
comp.binaries.ibm.pc.d
comp.binaries.ibm.pc.wanted
comp.binaries.mac
comp.binaries.ms-windows
comp.binaries.os2
ddn.mgt-bulletin
ddn.newsletter
ieee.announce
ieee.config
ieee.general
ieee.pcnfs
ieee.rab.announce
ieee.rab.general
ieee.region1
ieee.tab.announce
ieee.tab.general
ieee.tcos
ieee.usab.announce
ieee.usab.general
info.admin
info.big-internet
info.bind
info.brl-cad
info.bytecounters
info.convex
info.firearms
info.firearms.politics
info.gated
info.grass.programmer
info.grass.user
info.ietf
info.ietf.hosts
info.ietf.isoc
info.ietf.njm
info.ietf.smtp
info.isode
info.jethro-tull
info.labmgr
info.mach
info.mh.workers
info.nets
info.nsf.grants
info.nsfnet.cert
info.nsfnet.status
info.nupop
info.nysersnmp
info.osf
info.pem-dev
info.ph
info.rfc
info.slug
info.snmp
info.solbourne
info.sun-managers
info.sun-nets
info.theorynt
info.unix-sw
info.wisenet
k12.ed.art
k12.ed.business
k12.ed.comp.literacy
k12.ed.health-pe
k12.ed.life-skills
k12.ed.math
k12.ed.music
k12.ed.science
k12.ed.soc-studies
k12.ed.special
k12.ed.tag
k12.ed.tech
k12.lang.art
k12.lang.deutsch-eng
k12.lang.esp-eng
k12.lang.francais
k12.lang.russian
k12.library
k12.sys.channel0
k12.sys.channel1
k12.sys.channel10
k12.sys.channel11
k12.sys.channel12
k12.sys.channel2
k12.sys.channel3
k12.sys.channel4
k12.sys.channel5
k12.sys.channel6
k12.sys.channel7
k12.sys.channel8
k12.sys.channel9
k12.sys.projects
sco.opendesktop
trial.misc.legal.software
trial.soc.culture.czechoslovak
trial.soc.culture.italian
u3b.config
u3b.misc
u3b.sources
u3b.tech
u3b.test
vmsnet.admin
vmsnet.alpha
vmsnet.announce
vmsnet.announce.newusers
vmsnet.decus.journal
vmsnet.decus.lugs
vmsnet.employment
vmsnet.internals
vmsnet.mail.misc
vmsnet.mail.mx
vmsnet.mail.pmdf
vmsnet.misc
vmsnet.networks.desktop.misc
vmsnet.networks.desktop.pathworks
vmsnet.networks.management.decmcc
vmsnet.networks.management.misc
vmsnet.networks.misc
vmsnet.networks.tcp-ip.cmu-tek
vmsnet.networks.tcp-ip.misc
vmsnet.networks.tcp-ip.multinet
vmsnet.networks.tcp-ip.tcpware
vmsnet.networks.tcp-ip.ucx
vmsnet.networks.tcp-ip.wintcp
vmsnet.pdp-11
vmsnet.sources
vmsnet.sources.d
vmsnet.sources.games
vmsnet.sysmgt
vmsnet.test
vmsnet.tpu
vmsnet.uucp
vmsnet.vms-posix

groups needing moderation status changed to that shown:
alt.hotrod m
biz.sco.announce m
biz.sco.sources m
biz.zeos.announce m
comp.ai.vision m
comp.internet.library m
misc.news.east-europe.rferl m
rec.sport.cricket.scores m


1,,
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA28897; Fri, 30 Apr 1993 02:10:07 +0200 
Date: Fri, 30 Apr 1993 02:10:07 +0200
Message-Id: <199304300010.AA28897@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: B|g@lysator.liu.se (B|g i Svenskmud)
X-From-Mud-User: B|g i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Hur f}r man ...

*** EOOH ***
Date: Fri, 30 Apr 1993 02:10:07 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: B|g@lysator.liu.se (B|g i Svenskmud)
X-From-Mud-User: B|g i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Hur f}r man ...

Brev från B|g i SvenskMUD.
Till:   linus

hej, jag {r en simpel b|g som undrar hur man f}r kuk p} det h{r spelet?
{r det m|jligt att knulla n}gon h{r?
vad jag vill ha {r en STOR kuk i min utt{njda r|v...
skicka ett brev om du vet var jag hittar en STOR kuk!

		m}nga avsugningar!

						b|g


1,,
Received: from maxwell.lysator.liu.se (130.236.254.170) by lysator.liu.se (ALPHA-6.36/6.16) id AA15864; Fri, 30 Apr 1993 12:05:39 +0200 
Received: by maxwell.lysator.liu.se 
          (4.12/1.34/Lysator-3.1) id AA10023; Fri, 30 Apr 93 12:05:25 -0200
          (unknown)
Date: Fri, 30 Apr 93 12:05:25 -0200
From: linus@lysator.liu.se (Linus Tolke Y)
Message-Id: <9304301005.AA10023@maxwell.lysator.liu.se>
To: sojge@Minsk.DoCS.UU.SE
Cc: bug-lyskom@lysator.liu.se
In-Reply-To: Klaus Zeuge's message of Thu, 29 Apr 93 02:46:22 +0200 <9304290046.AA08667@Minsk.DoCS.UU.SE>

*** EOOH ***
Date: Fri, 30 Apr 93 12:05:25 -0200
From: linus@lysator.liu.se (Linus Tolke Y)
To: sojge@Minsk.DoCS.UU.SE
Cc: bug-lyskom@lysator.liu.se
In-Reply-To: Klaus Zeuge's message of Thu, 29 Apr 93 02:46:22 +0200 <9304290046.AA08667@Minsk.DoCS.UU.SE>

Din buggrapport {r mottagen.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Received: from godot.lysator.liu.se (130.236.254.154) by lysator.liu.se (ALPHA-6.36/6.16) id AA17324; Fri, 30 Apr 1993 12:31:39 +0200 
Received: by godot.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA00505; Fri, 30 Apr 93 12:31:37 +0200
          (unknown)
Date: Fri, 30 Apr 93 12:31:37 +0200
From: noppe@lysator.liu.se
Message-Id: <9304301031.AA00505@godot.lysator.liu.se>
To: news@lysator.liu.se
Subject: Vore det inte trevligt med nn p} maxwell && nanny ?

*** EOOH ***
Date: Fri, 30 Apr 93 12:31:37 +0200
From: noppe@lysator.liu.se
To: news@lysator.liu.se
Subject: Vore det inte trevligt med nn p} maxwell && nanny ?




1,,
Received: from quetzalcoatl.lysator.liu.se (130.236.254.133) by lysator.liu.se (ALPHA-6.36/6.16) id AA25593; Fri, 30 Apr 1993 23:42:46 +0200 
Received: by quetzalcoatl.lysator.liu.se 
          (5.51/1.34/Lysator-3.1) id AA02083; Fri, 30 Apr 93 23:42:50 +0200
          (unknown)
Date: Fri, 30 Apr 93 23:42:50 +0200
From: news@lysator.liu.se
Message-Id: <9304302142.AA02083@quetzalcoatl.lysator.liu.se>
Subject: possible active file problems
Apparently-To: news@lysator.liu.se

*** EOOH ***
Date: Fri, 30 Apr 93 23:42:50 +0200
From: news@lysator.liu.se
Subject: possible active file problems
Apparently-To: news@lysator.liu.se

If you believe this checkgroups control message, the following
differences may reflect groups that should be added, deleted,
or have their moderation status(es) changed:

obsolete groups:
de.alt.fan.tastische4

new groups:
de.alt.binaries.amigaos
de.alt.binaries.amigaos.d
de.alt.binaries.msdos
de.alt.binaries.msdos.d
de.alt.binaries.next
de.alt.binaries.pictures.comix
de.alt.binaries.pictures.d
de.alt.binaries.pictures.female
de.alt.binaries.pictures.male
de.alt.binaries.pictures.misc
de.alt.binaries.pictures.natur
de.alt.binaries.pictures.relay-party
de.alt.binaries.pictures.tech
de.alt.binaries.sounds
de.alt.binaries.sounds.d
de.alt.binaries.tos
de.alt.binaries.tos.d
de.soc.kontakte


1,,
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA26628; Sat, 1 May 1993 00:00:03 +0200 
Date: Sat, 1 May 1993 00:00:03 +0200
From: Linus Tolke Y <linus>
Message-Id: <199304302200.AA26628@lysator.liu.se>
To: linus
Subject: Couldn't run your "cron" job

*** EOOH ***
Date: Sat, 1 May 1993 00:00:03 +0200
From: Linus Tolke Y <linus>
To: linus
Subject: Couldn't run your "cron" job

Can't change directory to your home directory
The error was "I/O error"


1,,
Received: from quetzalcoatl.lysator.liu.se (130.236.254.133) by lysator.liu.se (ALPHA-6.36/6.16) id AA17961; Sat, 1 May 1993 06:26:44 +0200 
Received: by quetzalcoatl.lysator.liu.se 
          (5.51/1.34/Lysator-3.1) id AA09745; Sat, 1 May 93 05:59:18 +0200
          (unknown)
Message-Id: <9305010359.AA09745@quetzalcoatl.lysator.liu.se>
From: root@lysator.liu.se (Cron Daemon)
To: news@lysator.liu.se
Subject: cron for news@quetzalcoatl said this
Date: Sat May  1 06:00:00 1993
X-Cron-Cmd: </usr/lib/news/bin/runexpire>
X-Cron-Env: <SHELL=/bin/sh>
X-Cron-Env: <HOME=/usr/lib/news>
X-Cron-Env: <USER=news>

*** EOOH ***
From: root@lysator.liu.se (Cron Daemon)
To: news@lysator.liu.se
Subject: cron for news@quetzalcoatl said this
Date: Sat May  1 06:00:00 1993
X-Cron-Cmd: </usr/lib/news/bin/runexpire>
X-Cron-Env: <SHELL=/bin/sh>
X-Cron-Env: <HOME=/usr/lib/news>
X-Cron-Env: <USER=news>

Filesystem            kbytes    used   avail capacity  Mounted on
/dev/ioc/cdisk01h     433554  406400   18482    96%    /newsdisken
/dev/ioc/cdisk00f     396446  267390   89410    75%    /usr
Filesystem             iused   ifree  %iused  Mounted on
/dev/ioc/cdisk01h     123438   71122    63%   /newsdisken
/dev/ioc/cdisk00f      17439   68577    20%   /usr
startar expire     Sat May  1 05:59:17 MET DST 1993
startar updatemin  Sat May  1 05:59:33 MET DST 1993
expire klar        Sat May  1 06:26:15 MET DST 1993
Filesystem            kbytes    used   avail capacity  Mounted on
/dev/ioc/cdisk01h     433554  400392   24490    94%    /newsdisken
/dev/ioc/cdisk00f     396446  266916   89884    75%    /usr
Filesystem             iused   ifree  %iused  Mounted on
/dev/ioc/cdisk01h     121498   73062    62%   /newsdisken
/dev/ioc/cdisk00f      17440   68576    20%   /usr
startar nns expire


1,,
Received: from rudolf.lysator.liu.se (130.236.254.24) by lysator.liu.se (ALPHA-6.36/6.16) id AA06847; Sat, 1 May 1993 20:25:08 +0200 
Received: by rudolf.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA13437; Sat, 1 May 93 20:25:24 +0200
          (unknown)
Date: Sat, 1 May 93 20:25:22 MET DST
From: Magnus Redin <redin@lysator.liu.se>
To: linus@lysator.liu.se
Subject: [redin@lysator.liu.se: post failed]
Message-Id: <CMM.0.90.0.736280722.redin@rudolf.lysator.liu.se>

*** EOOH ***
Date: Sat, 1 May 93 20:25:22 MET DST
From: Magnus Redin <redin@lysator.liu.se>
To: linus@lysator.liu.se
Subject: [redin@lysator.liu.se: post failed]

Received: from rudolf.lysator.liu.se (130.236.254.24) by lysator.liu.se (ALPHA-6.36/6.16) id AA06025; Sat, 1 May 1993 20:10:30 +0200 
Received: by rudolf.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA13429; Sat, 1 May 93 20:10:46 +0200
          (unknown)
Date: Sat, 1 May 93 20:10:46 +0200
From: redin@lysator.liu.se
Message-Id: <9305011810.AA13429@rudolf.lysator.liu.se>
To: redin@lysator.liu.se
Subject: post failed

This machine does not have permission to use the quetzalcoatl news server.
/usr/local/lib/newsbin/inews failed

Your response has been saved in ~/dead.letter

Your article/letter follows:
Newsgroups: alt.necktie
Subject: Cetacean tie. (dolphines & whales)

I have bought my first necktie. An YSL silk tie with small orcas
on a reddish striped background (couldent it have been blue :( )
And I wonder if there are other ties with cetaceans to be found
anywhere?






1,,
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA12758; Sat, 1 May 1993 22:10:11 +0200 
Date: Sat, 1 May 1993 22:10:11 +0200
Message-Id: <199305012010.AA12758@lysator.liu.se>
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Inta@svmud.lysator.liu.se (Inta i Svenskmud)
Reply-To: magd-ahn@dsv.su.se (Inta i Svenskmud)
X-From-Mud-User: Inta i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Inta skriver brev igen..

*** EOOH ***
Date: Sat, 1 May 1993 22:10:11 +0200
To: linus@lysator.liu.se
X-To-Mud-User: linus i Svenskmud.
From: Inta@svmud.lysator.liu.se (Inta i Svenskmud)
Reply-To: magd-ahn@dsv.su.se (Inta i Svenskmud)
X-From-Mud-User: Inta i Svenskmud.
X-Postoffice: /rum/post@Svenskmud.lysator.liu.se
Subject: Inta skriver brev igen..

Brev från Inta i SvenskMUD.
Till:   linus

Hej Linus!
Nu har du f}tt din kvinnliga magiker..!
Jag t{nkte bara fr}ga om du kunde fixa s} att jag kan k|ra "ftp"
som Tetraput g|r.
Han sa att det var mycket b{ttre och tyckte att jag skulle skriva till
dej.
(Skulle var fint att kunna anv{nda emacs i byggandet)
Hej d}! Inta hulligan
*


1,,
Received: from quetzalcoatl.lysator.liu.se (130.236.254.133) by lysator.liu.se (ALPHA-6.36/6.16) id AA06839; Sun, 2 May 1993 05:22:27 +0200 
Received: by quetzalcoatl.lysator.liu.se 
          (5.51/1.34/Lysator-3.1) id AA00480; Sun, 2 May 93 05:22:38 +0200
          (unknown)
Date: Sun, 2 May 93 05:22:38 +0200
From: news@lysator.liu.se
Message-Id: <9305020322.AA00480@quetzalcoatl.lysator.liu.se>
Apparently-To: news@lysator.liu.se

*** EOOH ***
Date: Sun, 2 May 93 05:22:38 +0200
From: news@lysator.liu.se
Apparently-To: news@lysator.liu.se

unknown newsgroups by number of articles:
  10 alt.life.internet
   7 alt.tv.melrose-place
   7 alt.basement.graveyard
   5 alt.grad-student.tenured
   5 alt.freedom.of.information.act
   5 alt.fan.debbie.gibson
   4 news.test
   3 alt.tv.simpsons.itchy-scratchy
   3 alt.fan.lightbulbs
   2 vmsnet.sysmgt,comp.sys.vms
   2 misc.forsale.computer
   2 alt.music.jewish
   2 alt.med.cfs
   2 alt.homosexual
   2 alt.fan.schwaben
   1 soc.culture.australia
   1 rec.games.rpg
   1 r-node.test,tor.test,ont.test,news.test
   1 comp.mulitmedia
   1 comp.mail
   1 alt.znet.pc
   1 alt.tv.90120.sucks.sucks.sucks
   1 alt.life.sucks
   1 alt.law-enforcement
   1 alt.hackers.cough.cough.cough
   1 alt.great.ass.wheaton
   1 alt.fis
   1 alt.feet
   1 alt.fan.ren-and-stimpy
   1 alt.fan.mts
   1 alt.culture.theory
   1 alt.cesium
   1 alt.cable-tv.re-regulate
   1 alt.bbs.unixbbs.uniboard
   1 alt.anarchism

unsubscribed newsgroups:
  20 k12.chat.senior
   8 k12.ed.comp.literacy
   4 k12.library
   4 k12.ed.tech
   3 k12.sys.projects
   3 k12.sys.channel9
   3 k12.ed.science
   3 k12.chat.teacher
   2 k12.sys.channel0
   2 k12.lang.russian
   2 k12.lang.art
   2 k12.ed.math
   2 fj.rec.travel.japan.ctl
   1 vmsnet.uucp
   1 k12.sys.channel7
   1 k12.lang.francais
   1 k12.lang.deutsch-eng
   1 k12.ed.tag
   1 k12.ed.music
   1 k12.ed.health-pe
   1 k12.ed.art
   1 fj.sys.sun
   1 fj.rec.travel.world.ctl
   1 fj.rec.sports.baseball
   1 bit.admin.ctl
   1 alt.binaries.pictures.tasteless

sites sending stale/future/misdated news:
   5 dziuxsolim.rutgers.edu

sites sending news with bad headers:
   2 dziuxsolim.rutgers.edu
   1 pinus

sites with accepted news:
10249 isy
2222 kth.se
 253 pinus
 129 dziuxsolim.rutgers.edu
  79 lysator.liu.se



1,,
Received: from sune.stacken.kth.se (130.237.234.42; [130.237.234.42]) by lysator.liu.se (ALPHA-6.36/6.16) id AA12727; Sun, 2 May 1993 07:10:06 +0200 
Received: from linnea-grind.stacken.kth.se by sune.stacken.kth.se with SMTP id AA09452
  (5.65c/IDA-1.4.4 for <nucc@sune.stacken.kth.se>); Sun, 2 May 1993 07:09:49 +0200
Received: by linnea-grind.stacken.kth.se id AA09082
  (5.61-bind 1.4+ida/IDA-1.4.4); Sun, 2 May 93 07:09:48 +0200
Date: Sun, 2 May 93 07:09:47 +0200
From: Jan Michael Rynning <jmr@stacken.kth.se>
To: nucc@stacken.kth.se
Subject: NUCCC-93 
Message-Id: <CMM.0.88.736319387.jmr@linnea-grind.stacken.kth.se>

*** EOOH ***
Date: Sun, 2 May 93 07:09:47 +0200
From: Jan Michael Rynning <jmr@stacken.kth.se>
To: nucc@stacken.kth.se
Subject: NUCCC-93 

Since we have had problems with the news distribution previous years, I
both post this to nordunet.nucc and mail it to nucc@stacken.kth.se, the
Nordic University Computer Clubs mailinglist.  Please circulate this
among your members.


			N  U  C  C  C  -  9  3

		     in Stockholm, 11th-13th June


The Nordic University Computer Clubs Conference 1993 will be arranged
by the Stacken Computer Club, in Stockholm, 11th-13th June.  For
additional  information and registration, please send an email to
nuccc-93@stacken.kth.se.

Tietokoneyhdistus Stacken j{rjest{{ Pohjoismaitten korkeakoulujen
tietokoneyhdist|jen kokous 1993 Tukholmassa, 11-13 kes{kuuta. Jos
haluat tiet{{ lis{{ tai ilmoittautua kokoukseen, kirjoita (englanniksi
tai ruotsiksi, pyyd{mme) osoitteeseen nuccc-93@stacken.kth.se.  

Det nordiska h|gskoledatorf|reningsm|tet 1993 anordnas av Datorf|reningen
Stacken, i Stockholm, 11-13 juni. Om du vill veta mer eller vill anm{la
dig till m|tet, skicka ett email till nuccc-93@stacken.kth.se.

Programme:

Date and time  Location  Activity

Fri 11 Jun 12  KTH       Participands start to arrive, registration, ...
           18  KTH       We all gather and go to Osqvik by bus
               Osqvik    Dinner, sauna, sports, sauna, film, sauna, ...

Sat 12 Jun 07  Osqvik    ... breakfast, sauna, ...
           10  Osqvik    We return to KTH by bus
               KTH       Lecture, PDC, sightseeing, "real world MUD" :-),
                         hacking, ...
           18  City      Formal dinner at some restaurant in Stockholm, ...
               KTH       Accomodation will be provided in the student union
                         building

Sun 13 Jun 07  KTH       ... breakfast, sauna, sports, ...
           12  KTH       Bye-bye!  (Who will arrange NUCCC-94?)

We shall provide a floor to sleep on.  You should bring your sleeping bag
or arrange your accomodation yourself, if you don't want to sleep on the
floor.  Please remember to bring all umbrellas and raincoats you can find,
to make sure we get nice weather!


The conference fee will be SEK 350, to be payed when you arrive.  The fee
includes accomodation and the meals mentioned in the programme.  Please
register before Friday 21st May, so that we know well in advance how many
of you will come.  Email your registrations to nuccc-93@stacken.kth.se,
and include your name, email address, and the name of your club.  We shall
acknowledge all received registrations.


See you in Stockholm, 11th-13th June!


Jan Michael Rynning
Stacken Computer Club, Royal Institute of Technology, Stockholm, Sweden


1,, df,
Received: from quetzalcoatl.lysator.liu.se (130.236.254.133) by lysator.liu.se (ALPHA-6.36/6.16) id AA17543; Sun, 2 May 1993 08:39:16 +0200 
Received: by quetzalcoatl.lysator.liu.se 
          (5.51/1.34/Lysator-3.1) id AA00896; Sun, 2 May 93 05:59:09 +0200
          (unknown)
Message-Id: <9305020359.AA00896@quetzalcoatl.lysator.liu.se>
From: root@lysator.liu.se (Cron Daemon)
To: news@lysator.liu.se
Subject: cron for news@quetzalcoatl said this
Date: Sun May  2 06:00:00 1993
X-Cron-Cmd: </usr/lib/news/bin/runexpire>
X-Cron-Env: <SHELL=/bin/sh>
X-Cron-Env: <HOME=/usr/lib/news>
X-Cron-Env: <USER=news>

*** EOOH ***
From: root@lysator.liu.se (Cron Daemon)
To: news@lysator.liu.se
Subject: cron for news@quetzalcoatl said this
Date: Sun May  2 06:00:00 1993
X-Cron-Cmd: </usr/lib/news/bin/runexpire>
X-Cron-Env: <SHELL=/bin/sh>
X-Cron-Env: <HOME=/usr/lib/news>
X-Cron-Env: <USER=news>

Filesystem            kbytes    used   avail capacity  Mounted on
/dev/ioc/cdisk01h     433554  362916   61966    85%    /newsdisken
/dev/ioc/cdisk00f     396446  264188   92612    74%    /usr
Filesystem             iused   ifree  %iused  Mounted on
/dev/ioc/cdisk01h     108386   86174    56%   /newsdisken
/dev/ioc/cdisk00f      17387   68629    20%   /usr
startar expire     Sun May  2 05:59:08 MET DST 1993
startar updatemin  Sun May  2 08:33:53 MET DST 1993
expire klar        Sun May  2 08:39:17 MET DST 1993
Filesystem            kbytes    used   avail capacity  Mounted on
/dev/ioc/cdisk01h     433554  348596   76286    82%    /newsdisken
/dev/ioc/cdisk00f     396446  264046   92754    74%    /usr
Filesystem             iused   ifree  %iused  Mounted on
/dev/ioc/cdisk01h     103603   90957    53%   /newsdisken
/dev/ioc/cdisk00f      17399   68617    20%   /usr
startar nns expire


1,,
Received: from rudolf.lysator.liu.se (130.236.254.24) by lysator.liu.se (ALPHA-6.36/6.16) id AA01904; Sun, 2 May 1993 21:19:06 +0200 
Received: by rudolf.lysator.liu.se 
          (4.1/1.34/Lysator-3.1) id AA01686; Sun, 2 May 93 21:19:06 +0200
          (unknown)
Date: Sun, 2 May 93 21:19:06 +0200
From: linus@lysator.liu.se
Message-Id: <9305021919.AA01686@rudolf.lysator.liu.se>
To: d92ponha@und.ida.liu.se
In-Reply-To: Pontus Hagland's message of Sat, 1 May 93 15:23:47 +0200 <9305011323.AA02988@und.ida.liu.se>
Subject: /usr/svmud/lib/spelare

*** EOOH ***
Date: Sun, 2 May 93 21:19:06 +0200
From: linus@lysator.liu.se
To: d92ponha@und.ida.liu.se
In-Reply-To: Pontus Hagland's message of Sat, 1 May 93 15:23:47 +0200 <9305011323.AA02988@und.ida.liu.se>
Subject: /usr/svmud/lib/spelare

Fixat!
Om du vill att law skall vara {gare f}r du prata med n}gon root som
g|r chown.
-- 
	/Linus
*****	Wherever I exec my `which emacs`, is my $HOME.	*****
Linus Tolke				SM7OUU, linus@lysator.liu.se
Student at the				member of SK5EU LiTHSA
Link|ping institute of technology, LiTH	LiTH S{ndare Amat|rer (Ham-club)


1,,
Received: by lysator.liu.se (ALPHA-6.36/6.16) id AA08928; Sun, 2 May 1993 23:25:46 +0200 
Date: Sun, 2 May 1993 23:25:46 +0200
From: Per Cederqvist <ceder>
Message-Id: <199305022125.AA08928@lysator.liu.se>
To: linus

*** EOOH ***
Date: Sun, 2 May 1993 23:25:46 +0200
From: Per Cederqvist <ceder>
To: linus

(load-file "swedish-strings.el")
(setq a (copy-sequence lyskom-strings))
(load-file "english-strings.el")
(setq b (copy-sequence lyskom-strings))

(with-output-to-temp-buffer "*diffs*"
  (while (or a b)
    (cond
     ((eq (car (car a)) (car (car b)))
      (setq a (cdr a))
      (setq b (cdr b)))
     ((eq (cdr (car a)) 'found)
      (setq a (cdr a)))
     ((eq (cdr (car b)) 'founc)
      (setq b (cdr b)))
     ((assoc (car (car a)) b)
      (setcdr (assoc (car (car a)) b) 'found)
      (setq a (cdr a)))
     ((assoc (car (car b)) a)
      (setcdr (assoc (car (car b)) a) 'found)
      (setq b (cdr b)))
     (t
      (prin1 (format "%s -- %s" (car a) (car b)))
      (terpri)
      (setq a (cdr a))
      (setq b (cdr b))))))

