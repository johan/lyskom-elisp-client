<!-- -*-mode: xml-*- -->
<help language="sv">
  <section id="conferences" prompt="Veta mer om m�ten">

    <h1>M�ten</h1>

    <p>
      Varje LysKOM-system �r uppdelat i ett antal olika m�ten som alla
      behandlar olika �mnen. Vem som helst kan skapa m�ten, men man
      ska t�nka efter ordenligt innan man skapar ett m�te. I ett
      gammalt LysKOM-system s� finns det ofta redan m�ten som handlar
      om det man �r intresserad av.
    </p>

    <p>
      M�ten kan vara �ppna, slutna och hemliga. Vem som helst kan g�
      med i och l�sa inl�gg i alla �ppna m�ten. F�r att g� med i
      slutna m�ten m�ste m�tesorganisat�ren l�gga till medlemskapet.
      Hemliga m�ten g�r inte ens att se, och servern g�r sitt b�sta
      f�r att inte l�cka n�gon information om dessa. De flesta m�ten
      brukar vara �ppna. Slutna och hemliga m�ten �r mycket
      ovanligare.
    </p>

    <p>
      F�ljande kommandon kan vara av intresse f�r att komma ig�ng:
    </p>

    <inline id="kom-list-conferences" />
    <inline id="kom-review-presentation" />
    <inline id="kom-add-self" />
    <inline id="kom-go-to-conf" />

    <h2>Prioriteter</h2>

    <p>
      Dina medlemskap i m�ten har prioriteter. M�ten med h�gre
      prioritet kommer att presenteras f�re m�ten med l�gre prioritet
      n�r du loggar in. Anv�nd <cref="kom-prioritize" /> f�r att
      prioritera m�ten.
    </p>
  </section>

  <section id="persons" prompt="Veta mer om personer">
    <h1>Personer</h1>

    <p>
      Varje anv�ndare i LysKOM har en person. En person inneh�ller en
      del statistik om vad anv�ndaren har gjort (skapade inl�gg, l�sta
      inl�gg, n�rvarotid och en del annat). Varje person �r dessutom
      kopplad till en brevl�da med samma namn som personen. Brevl�dan
      �r i princip ett vanligt m�te, men fr�n b�rjan �r det slutet.
    </p>
  </section>

  <section id="texts" prompt="Veta mer om inl�gg">
    <h1>Inl�gg</h1>

    <p>
      Inl�gg �r LysKOMs syfte. Inl�gg �r helt enkelt texter skrivna av
      n�gon medlen i LysKOM.
    </p>

    <h2>Mottagare</h2>

    <p>
      Varje inl�gg har en eller flera mottagare; m�ten som inl�gget �r
      skickat till. Mottagare finns i tre olika smaker: vanlig
      mottagare, extra-kopiemottagare och dold mottagare.
    </p>

    <p>
      <b>Vanlig mottagare</b> �r det normala. En vanlig mottagare
      betyder att inl�gget h�r hemma i det angivna m�tet (eller i alla
      angivna m�ten). Kommentarer till inl�gget kommer automatiskt att
      skickas till samma mottagare.
    </p>

    <p>
      <b>Extra-kopiemottagare</b> anv�nds n�r man vill skicka en kopia
      av ett inl�gg till ett m�te utan att kommentarer till inl�gget
      skall hamna i m�tet.
    </p>

    <p>
      <b>Dold mottagare</b> �r ganska ovanligt. Bara den som har r�tt
      att vara medlem i m�tet som �r dold mottagare kan �verhuvudtaget
      se den dolda mottagaren. Detta kan allts� anv�ndas f�r att
      skicka inl�gg till n�gons brevl�da eller till ett slutet m�te
      utan att andra kan se det.
    </p>

    <h2>Kommentarer</h2>

    <p>
      Ett inl�gg kan ha kommentarer. Kommentarer till kommentarer till
      kommentarer bildar tr�dar, kommentarskedjor, kommentarstr�d och
      inl�ggstr�d. Vem som helst kan skriva kommentarer till vilket
      inl�gg som helst. Det finns inget krav p� att kommentarer ska
      skickas till samma m�ten som det som kommenteras. Det �r till
      exempel r�tt vanligt med kommentarer som bara skickas till
      f�rfattaren f�r det kommenterade inl�gget.
    </p>

    <h2>Speciella inl�gg</h2>

    <p>
      Det finns tre speciella inl�ggstyper: presentationer, FAQer och
      Lappar. En presentation �r ett inl�gg kopplat till ett m�te
      eller en brevl�da som inneh�ller en presentation av m�tet eller
      personen. Dessa ligger normalt i speciella m�ten. En FAQ �r ett
      inl�gg som svarar p� fr�gor som ofta st�lls i m�tet. Varje FAQ
      �r kopplad till ett eller flera m�ten, och man erbjuds att l�sa
      nya FAQer s� fort elispklienten ser att de har skapats. Lappar
      �r inl�gg som �r kopplade till m�ten, brevl�dor eller till hela
      servern som inneh�ller viktig information. Till exempel �r det
      vanligt att tala om att man reser bort och inte kan l�sa LysKOM
      genom att s�tta en lapp p� sin brevl�da.
    </p>
  </section>

  <section id="settings" prompt="Veta mer om inst�llningar">
    <h1>Inst�llningar</h1>

    <p>
      Elispklienten har ett stort antal inst�llningar. Med n�gra f�
      undantag kan man �ndra alla via kommandot <cref
      id="kom-customize" />.
    </p>

    <p>
      Inst�llningar kan sparas antingen i servern eller i din .emacs.
      Inst�llningar som sparas i servern g�ller per anv�ndare och
      endast f�r en session. Inst�llningar som sparas i .emacs g�ller
      i alla sessioner, oavsett om samma inst�llningar �ven har
      sparats i servern.
    </p>
  </section>

  <section id="reading" prompt="L�sa inl�gg">
    <h1>Att l�sa inl�gg</h1>

    <p>
      F�r att bara l�sa inl�gg i den ordning de presenteras i LysKOM
      r�cker det att trycka p� SPC. Ett inl�gg i taget kommer att
      visas tills det inte finns n�gra kvar. Det finns �ven ett antal
      mer avancerade kommandon. Anv�nd kommandot <cref
      id="kom-list-summary" /> f�r att se vilka inl�gg som ligger p�
      k� f�r att l�sas.
    </p>

    <refer id="filter" />
    <refer id="review" />
    <refer id="mark" />
  </section>

  <section id="filter" prompt="Slippa l�sa inl�gg">
    <h1>Kommandon f�r att slippa l�sa inl�gg</h1>

    <p>
      Ibland vill man inte l�sa en del inl�gg. D� kan f�ljande
      kommandon komma v�l till pass. F�rutom dessa finns ett antal
      kommandon f�r att filtrera inl�gg, om det �r n�gon typ av inl�gg
      som man aldrig vill se.
    </p>

    <inline id="kom-set-unread" />
    <inline id="kom-jump" />
    <inline id="kom-super-jump" />
  </section>

  <section id="mark" prompt="Komma ih�g inl�gg">
    <h1>Kommandon f�r att komma ih�g inl�gg</h1>

    <p>
      Ibland hittar man ett inl�gg som man vill komma ih�g. D� kan man
      markera inl�gget. I grund och botten �r varje markering ett
      heltal mellan 0 och 255, men med elispklienten kan man ge ett
      namn till varje typ av markering.
    </p>

    <inline id="kom-mark-text" />
    <inline id="kom-unmark-text" />
    <inline id="kom-review-all-marked-texts" />
    <inline id="kom-review-marked-texts" />
    <inline id="kom-list-marks" />
  </section>

  <section id="review" prompt="Hitta gamla inl�gg">
    <h1>Kommandon f�r att hitta gamla inl�gg</h1>

    <p>
      Ibland vill man hitta gamla inl�gg. Det finns ett stort antal
      kommadon f�r att g�ra detta, och alla har namn som b�rjar med
      "�terse".
    </p>

    <inline id="kom-review-by-to" />
    <inline id="kom-review-first" />
    <inline id="kom-review-more" />
    <inline id="kom-review-clear" />
    <inline id="kom-view-commented-text" />
    <inline id="kom-review-comments" />
    <inline id="kom-find-root-review" />
    <inline id="kom-find-root" />
    <inline id="kom-review-tree" />
  </section>

  <section id="writing" prompt="Skriva eller �ndra inl�gg">
    <h1>Att skriva eller �ndra inl�gg</h1>

    <p>
      Det finns i grund och botten tre sorters inl�gg: urinl�gg,
      kommentarer och fotnoter. Alla �r inl�gg, men de h�nger ihop med
      andra inl�gg p� olika s�tt.
    </p>

    <h2>Urinl�gg</h2>

    <p>
      Urinl�gg �r inl�gg som inte �r kommentarer till n�got annat
      inl�gg. Dessa inleder i allm�nhet nya diskussioner. Kom ih�g att
      skriva en vettig �renderad n�r du skriver nya urinl�gg. Brev �r
      vanliga inl�gg som skickas till n�gons brevl�da.
    </p>

    <inline id="kom-write-text" />
    <inline id="kom-send-letter" />

    <h2>Kommentarer</h2>

    <p>
      Kommentarer �r, som det l�ter, kommentarer till andra inl�gg.
      N�r du skriver en kommentar, kolla att �renderaden fortfarande
      �r vettig, och kolla att mottagarna till inl�gget �r vettiga.
      �ndra det som inte �r vettigt. Personliga svar �r kommentarer
      som skickas till f�rfattarens brevl�da. Det �r egentligen bara
      vanliga kommentarer d�r man har bytt mottagare.
    </p>

    <inline id="kom-write-comment" />
    <inline id="kom-private-answer" />

    <h2>Fotnoter</h2>

    <p>
      Fotnoter �r speciella kommentarer som bara f�rfattaren till ett
      inl�gg kan skriva. De visas f�re vanliga kommentarer och �r bra
      n�r man vill klarg�ra n�got man har skrivit i ett inl�gg s� att
      det visas innan folk b�rjar kommentera inl�gget.
    </p>

    <inline id="kom-write-footnote" />

    <h2>Prefixargument</h2>

    <p>
      Det finns flera s�tt att ange vilket inl�gg man vill kommentera
      eller skriva en fotnot till. N�r man ger n�got av ovanst�ende
      kommando utan extra argument s� g�ller kommandot det senaste
      inl�gg som man l�ste (f�r fotnoter det senaste man skrev eller
      l�ste som man var f�rfattare till).
    </p>

    <p>
      Ibland vill man ha en annan text. Dels finns kommandon som
      kommenterar den n�st sista text man l�ste (<cref
      id="kom-comment-previous" />, <cref
      id="kom-private-answer-previous" />). Dessutom finns m�jligheter
      att ge prefixargument till alla kommandon som hanterar inl�gg.
      Ange prefixargument som vanligt i Emacs genom C-u f�ljt av
      argumentets v�rde, eller genom att bara skriva in argumentets
      v�rde direkt.
    </p>

    <p>
      Ange ett textnummer <i>N</i> genom att ge inl�ggsnumret som
      prefixargument. Ange inl�gget som mark�ren st�r i genom att ge
      prefixargument 0. Ange inl�gget <i>N</i> inl�gg bak�t i
      bufferten fr�n d�r mark�ren st�r genom att ge argument
      -<i>N</i>. F� en prompt att mata in inl�ggsnumret vid genom att
      skriva C-u f�re kommandot.
    </p>

    <h2>Hantera mottagare</h2>

    <p>
      Ibland vill man �ndra mottagare p� ett inl�gg. Till det finns
      kommandona <cref id="kom-add-recipient" /> f�r att l�gga till
      och <cref id="kom-sub-recipient" /> f�r att ta bort mottagare.
    </p>

    <h2>Meddelanden och anm�rkningar</h2>

    <p>
      Anv�nd aldrig meddelanden eller anm�rkningar ist�llet f�r
      vanliga inl�gg. Det fungerar inte och folk blir sura.
    </p>
  </section>


  <section id="kom-list-conferences" prompt="">
    <h3><cref id="kom-list-conferences" /></h3>

    <p>
      Lista m�ten som finns i systemet. Anv�nd dessa med fantasifulla
      s�kbegrepp f�r att hitta m�ten som du �r intresserad av.
    </p>
  </section>

  <section id="kom-review-presentation" prompt="">
    <h3><cref id="kom-review-presentation" /></h3>

    <p>
      Visa presentationen f�r ett m�te eller en person. Anv�nd detta
      f�r att f� mer information om ett m�te.
    </p>
  </section>

  <section id="kom-add-self" prompt="">
    <h3><cref id="kom-add-self" /></h3>

    <p>
      G� med i ett m�te s� att du kan l�sa inl�ggen i m�tet och s� du
      f�r veta n�r det kommer nya inl�gg.
    </p>
  </section>

  <section id="kom-go-to-conf" prompt="">
    <h3><cref id="kom-go-to-conf" /></h3>

    <p>
      G� till ett m�te som du vill l�sa inl�gg i. Om du inte redan �r
      medlem i m�tet s� kommer du att erbjudas att bli medlem.
    </p>
  </section>

  <section id="kom-write-footnote" prompt="">
    <h3><cref id="kom-write-footnote" /></h3>

    <p>
      Skapar en ny fotnot till ett befintligt inl�gg.
    </p>
  </section>

  <section id="kom-private-answer" prompt="">
    <h3><cref id="kom-private-answer" /></h3>

    <p>
      Skapar ett personligt svar till ett befintligt inl�gg.
    </p>
  </section>

  <section id="kom-write-comment" prompt="">
    <h3><cref id="kom-write-comment" /></h3>

    <p>
      Skapar en ny kommentar till ett befintligt inl�gg.
    </p>
  </section>

  <section id="kom-send-letter" prompt="">
    <h3><cref id="kom-send-letter" /></h3>

    <p>
      Skapar ett nytt brev.
    </p>
  </section>

  <section id="kom-write-text" prompt="">
    <h3><cref id="kom-write-text" /></h3>

    <p>
      Skapar ett nytt urinl�gg.
    </p>
  </section>

  <section id="kom-review-tree" prompt="">
    <h3><cref id="kom-review-tree" /></h3>

    <p>
      �terser hela kommentarstr�det under det angivna inl�gget.
    </p>
  </section>

  <section id="kom-find-root" prompt="">
    <h3><cref id="kom-find-root" /></h3>

    <p>
      �terser urinl�gget till det angivna inl�gget.
    </p>
  </section>

  <section id="kom-find-root-review" prompt="">
    <h3><cref id="kom-find-root-review" /></h3>

    <p>
      �terser hela kommentarstr�det som det angivna inl�gget �r en del
      av. Ungef�r samma sak som att g�ra <cref id="kom-find-root" />
      f�ljt av <cref id="kom-review-tree" />.
    </p>
  </section>

  <section id="kom-review-comments" prompt="">
    <h3><cref id="kom-review-comments" /></h3>

    <p>
      �terser alla kommentarer till det angivna inl�gget, men inte
      kommentarer till kommentarerna.
    </p>
  </section>

  <section id="kom-view-commented-text" prompt="">
    <h3><cref id="kom-view-commented-text" /></h3>

    <p>
      �terser inl�gget som det angivna inl�gget �r en kommentar till.
    </p>
  </section>

  <section id="kom-review-clear" prompt="">
    <h3><cref id="kom-review-clear" /></h3>

    <p>
      Avbryter alla �tersekommandon som �r aktiva.
    </p>
  </section>

  <section id="kom-review-more" prompt="">
    <h3><cref id="kom-review-more" /></h3>

    <p>
      �terser fler inl�gg med samma kriterier som senaste <cref
      id="kom-review-by-to" />, eller <cref id="kom-review-first" />.
    </p>
  </section>

  <section id="kom-review-first" prompt="">
    <h3><cref id="kom-review-first" /></h3>

    <p>
      �terser de f�rsta inl�ggen skrivna av en viss person till ett
      visst m�te. Om du inte anger en viss parameter (eller s�tter
      antal till noll) s� tas ingen h�nsyn till den parametern, s� man
      kan �terse allt av en viss person, f�sta 5 av en viss person
      till alla m�ten och s� vidare.
    </p>
  </section>

  <section id="kom-review-by-to" prompt="">
    <h3><cref id="kom-review-by-to" /></h3>

    <p>
      �terser de senaste inl�ggen skrivna av en viss person till ett
      visst m�te. Om du inte anger en viss parameter (eller s�tter
      antal till noll) s� tas ingen h�nsyn till den parametern, s� man
      kan �terse allt av en viss person, f�sta 5 av en viss person
      till alla m�ten och s� vidare.
    </p>
  </section>

  <section id="kom-list-marks" prompt="">
    <h3><cref id="kom-list-marks" /></h3>

    <p>
      Listar alla inl�gg markerade med en viss markeringstyp.
    </p>
  </section>

  <section id="kom-review-marked-texts" prompt="">
    <h3><cref id="kom-review-marked-texts" /></h3>

    <p>
      �terser inl�gg markerade med en viss markeringstyp.
    </p>
  </section>

  <section id="kom-review-all-marked-texts" prompt="">
    <h3><cref id="kom-review-all-marked-texts" /></h3>

    <p>
      �terser alla markerade inl�gg.
    </p>
  </section>

  <section id="kom-unmark-text" prompt="">
    <h3><cref id="kom-unmark-text" /></h3>

    <p>
      Avmarkerar ett inl�gg. 
    </p>
  </section>

  <section id="kom-mark-text" prompt="">
    <h3><cref id="kom-mark-text" /></h3>

    <p>
      Markerar ett inl�gg. Fr�gar normalt efter inl�ggstyp.
    </p>
  </section>

  <section id="kom-super-jump" prompt="">
    <h3><cref id="kom-super-jump" /></h3>

    <p>
      Skapar ett filter som hoppar �ver inl�gg som har samma �renderad
      som det angivna inl�gget och som har aktuellt m�te som mottagare.
    </p>
  </section>

  <section id="kom-jump" prompt="">
    <h3><cref id="kom-jump" /></h3>

    <p>
      Hoppar �ver kommentarstr�det rotat i det senast l�sta inl�gget.
      Praktiskt n�r man ser att ett inl�gg inleder en diskussion som
      man inte �r intresserad av.
    </p>
  </section>

  <section id="kom-set-unread" prompt="">
    <h3><cref id="kom-set-unread" /></h3>

    <p>
      S�tter antalet ol�sta i aktuellt m�te. Kan vara bra om man har
      m�nga gamla inl�gg att l�sa ikapp eller helt enkelt inte �r
      intresserad av n�gonting i m�tet. 
    </p>
  </section>
</help>
