TPin - Základní propojovací třída. Spojení tříd pro proudové zpracování je prováděno pomocí tzv. pinů. Pro oba směry je vytvořen samostatný pin. 

TFrameLog - Třída záznamu proudu dat do textového souboru. Neovliňuje samotná data.

TCommFrame - třída kóduje rámce omezené velikostí do nepřetržitého proudu dat. Rámce jsou uvozeny symbolem začátku a ukončeny symbolem konce. Samotná data jsou překódována, aby nedocházelo k záměně obyčejných dat se speciální symboly.

TSerialPort - Zakončovací třída proudu dat, která s použitím třídy CommPort převádí proud data na sériovou linku.

TPacketBurst - Třída kóduje malé pakety do větších kvůli úspoře dat na záhlaví paketů. Zvyšuje velikost a kolísání zpoždění průchodu paketů.

TReliableConnection - Třída zajišťuje přenos paketů ověřovaných kontrolním kódem a opakovaní jejich přenosu při jejich ztrátě.
