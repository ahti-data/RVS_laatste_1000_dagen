# Wijzigingen aan een dashboard voorstellen (voor projectmanagers)

Je kunt met Claude zelf wijzigingen aan een dashboard maken en die op een preview-versie bekijken.
Het echte dashboard verandert pas als een data-analist jouw wijziging goedkeurt en samenvoegt.

## Eenmalig instellen

1. Zorg dat je Git en Claude Code op je laptop hebt en dat je toegang hebt tot de repository van het
   dashboard (vraag het anders aan een data-analist).
2. Clone de repository naar je laptop.
3. Draai in de map van de repository, één keer:

   ```powershell
   powershell -ExecutionPolicy Bypass -File .claude\pm\setup-pm.ps1
   ```

   Dit voegt veiligheidsregels toe aan je Claude Code: Claude kan dan niet naar `main` pushen, geen
   pull requests samenvoegen en niet aan de deploy-bestanden of de data komen. Start Claude Code daarna
   opnieuw.

## Een wijziging maken

1. **Maak een eigen branch. Werk nooit direct op `main`.**

   ```bash
   git switch main
   git pull
   git switch -c pm/korte-omschrijving
   ```

2. Vraag Claude om de wijziging, bijvoorbeeld "voeg een tweede grafiek toe met ...". Beschrijf wat je
   wilt zien, niet hoe het gemaakt moet worden.
3. Laat Claude de wijziging committen en pushen naar je branch:

   ```bash
   git push -u origin HEAD
   ```

4. Open een **pull request** naar `main`. GitHub toont na de push een knop "Compare & pull request".
   Geef de pull request een duidelijke titel: die staat ook op de previewpagina.

## Je wijziging bekijken

Na ongeveer twee minuten staat er onder je pull request een reactie met een link. Je kunt ook naar
**https://healthinsights.ahti.nl/preview/** gaan: daar staan alle previews, de nieuwste bovenaan.

- Log in met je gebruikelijke account.
- De preview gebruikt dezelfde (actuele) data als het echte dashboard.
- Push je nog een aanpassing naar dezelfde branch, dan wordt dezelfde preview ververst.
- Zodra de pull request is gesloten of samengevoegd, verdwijnt de preview. Elke nacht worden previews van
  gesloten pull requests bovendien opgeruimd, voor het geval dat niet direct lukte.

## Wat gebeurt er daarna

Een data-analist bekijkt je pull request, geeft eventueel commentaar en voegt hem samen. Pas dan is de
wijziging zichtbaar in het echte dashboard. Reageer op commentaar door Claude de aanpassing te laten
maken en opnieuw te pushen.

## Wat je niet kunt en niet moet doen

- Niet werken op of pushen naar `main`; niet zelf een pull request samenvoegen.
- Niet aanpassen: `dashboard/data/`, `outputs/`, `output_src/`, `.github/`. Daar gaan data-analisten over. Een wijziging in de
  data is ook niet zichtbaar in een preview, want die toont altijd de actuele data.
- Neem geen persoonsgegevens of herleidbare cijfers op in commits, pull requests of berichten aan
  Claude. Twijfel je of iets herleidbaar is, vraag het dan aan een data-analist.
- Deze regels beperken alleen Claude. Ze houden je niet tegen als je zelf `git`-opdrachten typt, dus
  ook daar geldt: nooit op `main` werken.
