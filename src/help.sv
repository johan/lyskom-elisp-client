<!-- -*-mode: xml-*- -->
<help language="sv">
  <section id="conferences" prompt="Veta mer om möten">

    <h1>Möten</h1>

    <p>
      Varje LysKOM-system är uppdelat i ett antal olika möten som alla
      behandlar olika ämnen. Vem som helst kan skapa möten, men man
      ska tänka efter ordenligt innan man skapar ett möte. I ett
      gammalt LysKOM-system så finns det ofta redan möten som handlar
      om det man är intresserad av.
    </p>

    <p>
      Möten kan vara öppna, slutna och hemliga. Vem som helst kan gå
      med i och läsa inlägg i alla öppna möten. För att gå med i
      slutna möten måste mötesorganisatören lägga till medlemskapet.
      Hemliga möten går inte ens att se, och servern gör sitt bästa
      för att inte läcka någon information om dessa. De flesta möten
      brukar vara öppna. Slutna och hemliga möten är mycket
      ovanligare.
    </p>

    <p>
      Följande kommandon kan vara av intresse för att komma igång:
    </p>

    <inline id="kom-list-conferences" />
    <inline id="kom-review-presentation" />
    <inline id="kom-add-self" />
    <inline id="kom-go-to-conf" />

    <h2>Prioriteter</h2>

    <p>
      Dina medlemskap i möten har prioriteter. Möten med högre
      prioritet kommer att presenteras före möten med lägre prioritet
      när du loggar in. Använd <cref="kom-prioritize" /> för att
      prioritera möten.
    </p>
  </section>

  <section id="persons" prompt="Veta mer om personer">
    <h1>Personer</h1>

    <p>
      Varje användare i LysKOM har en person. En person innehåller en
      del statistik om vad användaren har gjort (skapade inlägg, lästa
      inlägg, närvarotid och en del annat). Varje person är dessutom
      kopplad till en brevlåda med samma namn som personen. Brevlådan
      är i princip ett vanligt möte, men från början är det slutet.
    </p>
  </section>

  <section id="texts" prompt="Veta mer om inlägg">
    <h1>Inlägg</h1>

    <p>
      Inlägg är LysKOMs syfte. Inlägg är helt enkelt texter skrivna av
      någon medlen i LysKOM.
    </p>

    <h2>Mottagare</h2>

    <p>
      Varje inlägg har en eller flera mottagare; möten som inlägget är
      skickat till. Mottagare finns i tre olika smaker: vanlig
      mottagare, extra-kopiemottagare och dold mottagare.
    </p>

    <p>
      <b>Vanlig mottagare</b> är det normala. En vanlig mottagare
      betyder att inlägget hör hemma i det angivna mötet (eller i alla
      angivna möten). Kommentarer till inlägget kommer automatiskt att
      skickas till samma mottagare.
    </p>

    <p>
      <b>Extra-kopiemottagare</b> används när man vill skicka en kopia
      av ett inlägg till ett möte utan att kommentarer till inlägget
      skall hamna i mötet.
    </p>

    <p>
      <b>Dold mottagare</b> är ganska ovanligt. Bara den som har rätt
      att vara medlem i mötet som är dold mottagare kan överhuvudtaget
      se den dolda mottagaren. Detta kan alltså användas för att
      skicka inlägg till någons brevlåda eller till ett slutet möte
      utan att andra kan se det.
    </p>

    <h2>Kommentarer</h2>

    <p>
      Ett inlägg kan ha kommentarer. Kommentarer till kommentarer till
      kommentarer bildar trådar, kommentarskedjor, kommentarsträd och
      inläggsträd. Vem som helst kan skriva kommentarer till vilket
      inlägg som helst. Det finns inget krav på att kommentarer ska
      skickas till samma möten som det som kommenteras. Det är till
      exempel rätt vanligt med kommentarer som bara skickas till
      författaren för det kommenterade inlägget.
    </p>

    <h2>Speciella inlägg</h2>

    <p>
      Det finns tre speciella inläggstyper: presentationer, FAQer och
      Lappar. En presentation är ett inlägg kopplat till ett möte
      eller en brevlåda som innehåller en presentation av mötet eller
      personen. Dessa ligger normalt i speciella möten. En FAQ är ett
      inlägg som svarar på frågor som ofta ställs i mötet. Varje FAQ
      är kopplad till ett eller flera möten, och man erbjuds att läsa
      nya FAQer så fort elispklienten ser att de har skapats. Lappar
      är inlägg som är kopplade till möten, brevlådor eller till hela
      servern som innehåller viktig information. Till exempel är det
      vanligt att tala om att man reser bort och inte kan läsa LysKOM
      genom att sätta en lapp på sin brevlåda.
    </p>
  </section>

  <section id="settings" prompt="Veta mer om inställningar">
    <h1>Inställningar</h1>

    <p>
      Elispklienten har ett stort antal inställningar. Med några få
      undantag kan man ändra alla via kommandot <cref
      id="kom-customize" />.
    </p>

    <p>
      Inställningar kan sparas antingen i servern eller i din .emacs.
      Inställningar som sparas i servern gäller per användare och
      endast för en session. Inställningar som sparas i .emacs gäller
      i alla sessioner, oavsett om samma inställningar även har
      sparats i servern.
    </p>
  </section>

  <section id="reading" prompt="Läsa inlägg">
    <h1>Att läsa inlägg</h1>

    <p>
      För att bara läsa inlägg i den ordning de presenteras i LysKOM
      räcker det att trycka på SPC. Ett inlägg i taget kommer att
      visas tills det inte finns några kvar. Det finns även ett antal
      mer avancerade kommandon. Använd kommandot <cref
      id="kom-list-summary" /> för att se vilka inlägg som ligger på
      kö för att läsas.
    </p>

    <refer id="filter" />
    <refer id="review" />
    <refer id="mark" />
  </section>

  <section id="filter" prompt="Slippa läsa inlägg">
    <h1>Kommandon för att slippa läsa inlägg</h1>

    <p>
      Ibland vill man inte läsa en del inlägg. Då kan följande
      kommandon komma väl till pass. Förutom dessa finns ett antal
      kommandon för att filtrera inlägg, om det är någon typ av inlägg
      som man aldrig vill se.
    </p>

    <inline id="kom-set-unread" />
    <inline id="kom-jump" />
    <inline id="kom-super-jump" />
  </section>

  <section id="mark" prompt="Komma ihåg inlägg">
    <h1>Kommandon för att komma ihåg inlägg</h1>

    <p>
      Ibland hittar man ett inlägg som man vill komma ihåg. Då kan man
      markera inlägget. I grund och botten är varje markering ett
      heltal mellan 0 och 255, men med elispklienten kan man ge ett
      namn till varje typ av markering.
    </p>

    <inline id="kom-mark-text" />
    <inline id="kom-unmark-text" />
    <inline id="kom-review-all-marked-texts" />
    <inline id="kom-review-marked-texts" />
    <inline id="kom-list-marks" />
  </section>

  <section id="review" prompt="Hitta gamla inlägg">
    <h1>Kommandon för att hitta gamla inlägg</h1>

    <p>
      Ibland vill man hitta gamla inlägg. Det finns ett stort antal
      kommadon för att göra detta, och alla har namn som börjar med
      "Återse".
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

  <section id="writing" prompt="Skriva eller ändra inlägg">
    <h1>Att skriva eller ändra inlägg</h1>

    <p>
      Det finns i grund och botten tre sorters inlägg: urinlägg,
      kommentarer och fotnoter. Alla är inlägg, men de hänger ihop med
      andra inlägg på olika sätt.
    </p>

    <h2>Urinlägg</h2>

    <p>
      Urinlägg är inlägg som inte är kommentarer till något annat
      inlägg. Dessa inleder i allmänhet nya diskussioner. Kom ihåg att
      skriva en vettig ärenderad när du skriver nya urinlägg. Brev är
      vanliga inlägg som skickas till någons brevlåda.
    </p>

    <inline id="kom-write-text" />
    <inline id="kom-send-letter" />

    <h2>Kommentarer</h2>

    <p>
      Kommentarer är, som det låter, kommentarer till andra inlägg.
      När du skriver en kommentar, kolla att ärenderaden fortfarande
      är vettig, och kolla att mottagarna till inlägget är vettiga.
      Ändra det som inte är vettigt. Personliga svar är kommentarer
      som skickas till författarens brevlåda. Det är egentligen bara
      vanliga kommentarer där man har bytt mottagare.
    </p>

    <inline id="kom-write-comment" />
    <inline id="kom-private-answer" />

    <h2>Fotnoter</h2>

    <p>
      Fotnoter är speciella kommentarer som bara författaren till ett
      inlägg kan skriva. De visas före vanliga kommentarer och är bra
      när man vill klargöra något man har skrivit i ett inlägg så att
      det visas innan folk börjar kommentera inlägget.
    </p>

    <inline id="kom-write-footnote" />

    <h2>Prefixargument</h2>

    <p>
      Det finns flera sätt att ange vilket inlägg man vill kommentera
      eller skriva en fotnot till. När man ger något av ovanstående
      kommando utan extra argument så gäller kommandot det senaste
      inlägg som man läste (för fotnoter det senaste man skrev eller
      läste som man var författare till).
    </p>

    <p>
      Ibland vill man ha en annan text. Dels finns kommandon som
      kommenterar den näst sista text man läste (<cref
      id="kom-comment-previous" />, <cref
      id="kom-private-answer-previous" />). Dessutom finns möjligheter
      att ge prefixargument till alla kommandon som hanterar inlägg.
      Ange prefixargument som vanligt i Emacs genom C-u följt av
      argumentets värde, eller genom att bara skriva in argumentets
      värde direkt.
    </p>

    <p>
      Ange ett textnummer <i>N</i> genom att ge inläggsnumret som
      prefixargument. Ange inlägget som markören står i genom att ge
      prefixargument 0. Ange inlägget <i>N</i> inlägg bakåt i
      bufferten från där markören står genom att ge argument
      -<i>N</i>. Få en prompt att mata in inläggsnumret vid genom att
      skriva C-u före kommandot.
    </p>

    <h2>Hantera mottagare</h2>

    <p>
      Ibland vill man ändra mottagare på ett inlägg. Till det finns
      kommandona <cref id="kom-add-recipient" /> för att lägga till
      och <cref id="kom-sub-recipient" /> för att ta bort mottagare.
    </p>

    <h2>Meddelanden och anmärkningar</h2>

    <p>
      Använd aldrig meddelanden eller anmärkningar istället för
      vanliga inlägg. Det fungerar inte och folk blir sura.
    </p>
  </section>


  <section id="kom-list-conferences" prompt="">
    <h3><cref id="kom-list-conferences" /></h3>

    <p>
      Lista möten som finns i systemet. Använd dessa med fantasifulla
      sökbegrepp för att hitta möten som du är intresserad av.
    </p>
  </section>

  <section id="kom-review-presentation" prompt="">
    <h3><cref id="kom-review-presentation" /></h3>

    <p>
      Visa presentationen för ett möte eller en person. Använd detta
      för att få mer information om ett möte.
    </p>
  </section>

  <section id="kom-add-self" prompt="">
    <h3><cref id="kom-add-self" /></h3>

    <p>
      Gå med i ett möte så att du kan läsa inläggen i mötet och så du
      får veta när det kommer nya inlägg.
    </p>
  </section>

  <section id="kom-go-to-conf" prompt="">
    <h3><cref id="kom-go-to-conf" /></h3>

    <p>
      Gå till ett möte som du vill läsa inlägg i. Om du inte redan är
      medlem i mötet så kommer du att erbjudas att bli medlem.
    </p>
  </section>

  <section id="kom-write-footnote" prompt="">
    <h3><cref id="kom-write-footnote" /></h3>

    <p>
      Skapar en ny fotnot till ett befintligt inlägg.
    </p>
  </section>

  <section id="kom-private-answer" prompt="">
    <h3><cref id="kom-private-answer" /></h3>

    <p>
      Skapar ett personligt svar till ett befintligt inlägg.
    </p>
  </section>

  <section id="kom-write-comment" prompt="">
    <h3><cref id="kom-write-comment" /></h3>

    <p>
      Skapar en ny kommentar till ett befintligt inlägg.
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
      Skapar ett nytt urinlägg.
    </p>
  </section>

  <section id="kom-review-tree" prompt="">
    <h3><cref id="kom-review-tree" /></h3>

    <p>
      Återser hela kommentarsträdet under det angivna inlägget.
    </p>
  </section>

  <section id="kom-find-root" prompt="">
    <h3><cref id="kom-find-root" /></h3>

    <p>
      Återser urinlägget till det angivna inlägget.
    </p>
  </section>

  <section id="kom-find-root-review" prompt="">
    <h3><cref id="kom-find-root-review" /></h3>

    <p>
      Återser hela kommentarsträdet som det angivna inlägget är en del
      av. Ungefär samma sak som att göra <cref id="kom-find-root" />
      följt av <cref id="kom-review-tree" />.
    </p>
  </section>

  <section id="kom-review-comments" prompt="">
    <h3><cref id="kom-review-comments" /></h3>

    <p>
      Återser alla kommentarer till det angivna inlägget, men inte
      kommentarer till kommentarerna.
    </p>
  </section>

  <section id="kom-view-commented-text" prompt="">
    <h3><cref id="kom-view-commented-text" /></h3>

    <p>
      Återser inlägget som det angivna inlägget är en kommentar till.
    </p>
  </section>

  <section id="kom-review-clear" prompt="">
    <h3><cref id="kom-review-clear" /></h3>

    <p>
      Avbryter alla återsekommandon som är aktiva.
    </p>
  </section>

  <section id="kom-review-more" prompt="">
    <h3><cref id="kom-review-more" /></h3>

    <p>
      Återser fler inlägg med samma kriterier som senaste <cref
      id="kom-review-by-to" />, eller <cref id="kom-review-first" />.
    </p>
  </section>

  <section id="kom-review-first" prompt="">
    <h3><cref id="kom-review-first" /></h3>

    <p>
      Återser de första inläggen skrivna av en viss person till ett
      visst möte. Om du inte anger en viss parameter (eller sätter
      antal till noll) så tas ingen hänsyn till den parametern, så man
      kan återse allt av en viss person, fösta 5 av en viss person
      till alla möten och så vidare.
    </p>
  </section>

  <section id="kom-review-by-to" prompt="">
    <h3><cref id="kom-review-by-to" /></h3>

    <p>
      Återser de senaste inläggen skrivna av en viss person till ett
      visst möte. Om du inte anger en viss parameter (eller sätter
      antal till noll) så tas ingen hänsyn till den parametern, så man
      kan återse allt av en viss person, fösta 5 av en viss person
      till alla möten och så vidare.
    </p>
  </section>

  <section id="kom-list-marks" prompt="">
    <h3><cref id="kom-list-marks" /></h3>

    <p>
      Listar alla inlägg markerade med en viss markeringstyp.
    </p>
  </section>

  <section id="kom-review-marked-texts" prompt="">
    <h3><cref id="kom-review-marked-texts" /></h3>

    <p>
      Återser inlägg markerade med en viss markeringstyp.
    </p>
  </section>

  <section id="kom-review-all-marked-texts" prompt="">
    <h3><cref id="kom-review-all-marked-texts" /></h3>

    <p>
      Återser alla markerade inlägg.
    </p>
  </section>

  <section id="kom-unmark-text" prompt="">
    <h3><cref id="kom-unmark-text" /></h3>

    <p>
      Avmarkerar ett inlägg. 
    </p>
  </section>

  <section id="kom-mark-text" prompt="">
    <h3><cref id="kom-mark-text" /></h3>

    <p>
      Markerar ett inlägg. Frågar normalt efter inläggstyp.
    </p>
  </section>

  <section id="kom-super-jump" prompt="">
    <h3><cref id="kom-super-jump" /></h3>

    <p>
      Skapar ett filter som hoppar över inlägg som har samma ärenderad
      som det angivna inlägget och som har aktuellt möte som mottagare.
    </p>
  </section>

  <section id="kom-jump" prompt="">
    <h3><cref id="kom-jump" /></h3>

    <p>
      Hoppar över kommentarsträdet rotat i det senast lästa inlägget.
      Praktiskt när man ser att ett inlägg inleder en diskussion som
      man inte är intresserad av.
    </p>
  </section>

  <section id="kom-set-unread" prompt="">
    <h3><cref id="kom-set-unread" /></h3>

    <p>
      Sätter antalet olästa i aktuellt möte. Kan vara bra om man har
      många gamla inlägg att läsa ikapp eller helt enkelt inte är
      intresserad av någonting i mötet. 
    </p>
  </section>
</help>
