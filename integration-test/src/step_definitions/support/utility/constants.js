const FIRST_NAME_SET = [
    "Aldo", "Alessia", "Alessio", "Alessandra", "Andrea", "Antonio", "Antonella", "Angelo", "Davide", "Daniela", "Enrico", "Elisa", "Erasmo", "Federico", "Francesca", 
    "Francesco", "Giacomo", "Ginevra", "Giovanni", "Giuseppe", "Jacopo", "Jessica", "Michele", "Miriana", "Luca", "Pasquale", "Patrizia", "Pietro", "Simone", "Simonetta"
];

const LAST_NAME_SET = [
    "Rossi", "Russo", "Ferrari", "Esposito", "Bianchi", "Romano", "Colombo", "Bruno", "Ricci", "Marino", "Costa", "Franco", "Gallo", "Conti", "Greco", "Martino", "Giordano",
    "Rizzo", "Mancini", "Villa", "Mauro", "Lombardi", "Fontana", "Roberto", "Barbieri", "Moretti", "Bianco", "Martini", "Mariani", "Rinaldi", "Amato", "Galli", "Ferrara", 
    "Caruso", "Leone", "Santoro", "Longo", "Sala", "Martinelli", "Serra", "Marchetti"
];

const CITIES_SET = [
    { city: "Roma", province: "RM", region: "Lazio", postalCode: "00120" },
    { city: "Milano", province: "MI", region: "Lombardia", postalCode: "20120" },
    { city: "Napoli", province: "NA", region: "Campania", postalCode: "80120" },
    { city: "Torino", province: "TO", region: "Piemonte", postalCode: "10120" },
    { city: "Palermo", province: "PA", region: "Sicilia", postalCode: "90120" },
    { city: "Genova", province: "GE", region: "Liguria", postalCode: "16120" },
    { city: "Bologna", province: "BO", region: "Emilia-Romagna", postalCode: "40210" },
    { city: "Firenze", province: "FI", region: "Toscana", postalCode: "50100" },
    { city: "Bari", province: "BA", region: "Puglia", postalCode: "70120" }
];

const STREET_SET = [
    "Via Garibaldi", "Via Mazzini", "Via Cavour", "Via dei Mille", "Via Principe Amedeo", "Via duca d'Abruzzi", "Viale della Resistenza", "Via Verdi", "Via Vittorio Emanuele I",
    "Via Pasolini", "Via Petrarca", "Via Manzoni", "Via Marconi", "Via Fermi", "Via De Gasperi", "Corso Italia", "Via Roma", "Via Unità di Italia", "Via Giulio Cesare", "Via dei Papi",
    "Via Leonardo da Vinci", "Via Francia", "Corso Europa", "Viale dei Caduti"
];

const COMPANY_SET = [
    "Aigos S.p.A.", "MaxiWorms Inc.", "Fratelli Cauretto", "Premiata Salumeria Goldaniga", "OctoLollipop", "Agenzia Immobiliare DeSantis", "Studi associati Conte", "Falegnameria e Poltrone",
    "Money Inc.", "InfoTool S.p.A", "Marionette S.r.l", "Studio medico Gastani-Frizzi", "Facetablet Inc", "Grobemuller Ght", "Dainelli immobiliare", "F.C. Real Italia", "Moda&Scarpe",
    "Notch Creation Corp"
];

module.exports = {
    CITIES_SET,
    COMPANY_SET,
    FIRST_NAME_SET,
    LAST_NAME_SET,
    STREET_SET
}