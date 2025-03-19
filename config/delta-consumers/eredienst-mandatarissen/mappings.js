/*
 * These rules were taken from mappings set up via a reasoner to transform data
 * from a model used in Loket to a model used in OP. The rules were swapped
 * around to transform the data into Loket.
 */

const mappingsPredicates = [
  {
    original: '<http://www.w3.org/ns/org#classification>',
    loket: '<http://data.vlaanderen.be/ns/besluit#classificatie>',
  },
  {
    //This rule was backwards? The model in Loket requires lower case 'v'. Is
    //the capital case ever used?
    original:
      '<https://data.vlaanderen.be/ns/adres#AdresVoorstelling.huisnummer>',
    loket: '<https://data.vlaanderen.be/ns/adres#Adresvoorstelling.huisnummer>',
  },
  {
    //Same
    original:
      '<https://data.vlaanderen.be/ns/adres#AdresVoorstelling.busnummer>',
    loket: '<https://data.vlaanderen.be/ns/adres#Adresvoorstelling.busnummer>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/generiek#isTijdspecialisatieVan>',
    loket: '<http://data.vlaanderen.be/ns/mandaat#isTijdspecialisatieVan>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#afstammingsType>',
    loket: '<http://data.vlaanderen.be/ns/persoon#afstammingsType>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#alternatieveNaam>',
    loket: '<http://data.vlaanderen.be/ns/persoon#alternatieveNaam>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#datum>',
    loket: '<http://data.vlaanderen.be/ns/persoon#datum>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#datumVanAfstamming>',
    loket: '<http://data.vlaanderen.be/ns/persoon#datumVanAfstamming>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#gebruikteVoornaam>',
    loket: '<http://data.vlaanderen.be/ns/persoon#gebruikteVoornaam>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#geslacht>',
    loket: '<http://data.vlaanderen.be/ns/persoon#geslacht>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#gezinsadres>',
    loket: '<http://data.vlaanderen.be/ns/persoon#gezinsadres>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#gezinsrelatietype>',
    loket: '<http://data.vlaanderen.be/ns/persoon#gezinsrelatietype>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#heeftBurgerlijkeStaat>',
    loket: '<http://data.vlaanderen.be/ns/persoon#heeftBurgerlijkeStaat>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#heeftGeboorte>',
    loket: '<http://data.vlaanderen.be/ns/persoon#heeftGeboorte>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#heeftInwonerschap>',
    loket: '<http://data.vlaanderen.be/ns/persoon#heeftInwonerschap>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#heeftNationaliteit>',
    loket: '<http://data.vlaanderen.be/ns/persoon#heeftNationaliteit>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#heeftOverlijden>',
    loket: '<http://data.vlaanderen.be/ns/persoon#heeftOverlijden>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#heeftPersoonsrelatie>',
    loket: '<http://data.vlaanderen.be/ns/persoon#heeftPersoonsrelatie>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#heeftStaatsburgerschap>',
    loket: '<http://data.vlaanderen.be/ns/persoon#heeftStaatsburgerschap>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#heeftVerblijfplaats>',
    loket: '<http://data.vlaanderen.be/ns/persoon#heeftVerblijfplaats>',
  },
  {
    original:
      '<https://data.vlaanderen.be/ns/persoon#Inwonerschap.binnenJurisdictie>',
    loket:
      '<http://data.vlaanderen.be/ns/persoon#Inwonerschap.binnenJurisdictie>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#isHoofdVan>',
    loket: '<http://data.vlaanderen.be/ns/persoon#isHoofdVan>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#isLidVan>',
    loket: '<http://data.vlaanderen.be/ns/persoon#isLidVan>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#isRelatieMet>',
    loket: '<http://data.vlaanderen.be/ns/persoon#isRelatieMet>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#land>',
    loket: '<http://data.vlaanderen.be/ns/persoon#land>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#nationaliteit>',
    loket: '<http://data.vlaanderen.be/ns/persoon#nationaliteit>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#plaats>',
    loket: '<http://data.vlaanderen.be/ns/persoon#plaats>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#registratie>',
    loket: '<http://data.vlaanderen.be/ns/persoon#registratie>',
  },
  {
    original:
      '<https://data.vlaanderen.be/ns/persoon#Staatsburgerschap.binnenJurisdictie>',
    loket:
      '<http://data.vlaanderen.be/ns/persoon#Staatsburgerschap.binnenJurisdictie>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#type>',
    loket: '<http://data.vlaanderen.be/ns/persoon#type>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#verblijfsadres>',
    loket: '<http://data.vlaanderen.be/ns/persoon#verblijfsadres>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#volledigeNaam>',
    loket: '<http://data.vlaanderen.be/ns/persoon#volledigeNaam>',
  },
  {
    original: '<http://data.vlaanderen.be/ns/persoon#registratie>',
    loket: '<http://www.w3.org/ns/adms#identifier>',
  },
];

// CLASSES
const mappingsClasses = [
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Afstamming>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Afstamming>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#BurgerlijkeStaat>',
    loket: '<http://data.vlaanderen.be/ns/persoon#BurgerlijkeStaat>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Domicilie>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Domicilie>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Geboorte>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Geboorte>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#GeenInwoner>',
    loket: '<http://data.vlaanderen.be/ns/persoon#GeenInwoner>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#GeregistreerdPersoon>',
    loket: '<http://data.vlaanderen.be/ns/persoon#GeregistreerdPersoon>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Gezin>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Gezin>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Gezinsrelatie>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Gezinsrelatie>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Huwelijk>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Huwelijk>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Inwoner>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Inwoner>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Inwonerschap>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Inwonerschap>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Nationaliteit>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Nationaliteit>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Overlijden>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Overlijden>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#PermanentInwoner>',
    loket: '<http://data.vlaanderen.be/ns/persoon#PermanentInwoner>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Persoonsgebeurtenis>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Persoonsgebeurtenis>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Persoonsrelatie>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Persoonsrelatie>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Samenwonen>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Samenwonen>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Staatsburger>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Staatsburger>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Staatsburgerschap>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Staatsburgerschap>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#TijdelijkInwoner>',
    loket: '<http://data.vlaanderen.be/ns/persoon#TijdelijkInwoner>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Verblijfplaats>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Verblijfplaats>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Voogdij>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Voogdij>',
  },
  {
    original: '<https://data.vlaanderen.be/ns/persoon#Vreemdeling>',
    loket: '<http://data.vlaanderen.be/ns/persoon#Vreemdeling>',
  },
];

module.exports = {
  mappingsPredicates,
  mappingsClasses,
};
