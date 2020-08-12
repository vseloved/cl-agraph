;;; CL-AGRAPH symbolic SPARQL translator test
;;; (c) Vsevolod Dyomkin. see LICENSE file for permissions

(in-package :agraph)
(named-readtables:in-readtable rutils-readtable)


(defmethod should-check ((key (eql :match-except-ws)) expected fn &rest _)
  (declare (ignore _))
  (with ((raw (call fn))
         (rez (re:regex-replace-all "\\s+" raw "")))
    (or (string= rez (re:regex-replace-all "\\s+" expected ""))
        (values nil
                raw))))

(deftest simple ()
  (should match-except-ws "
SELECT * { ?S ?P ?O . }"
          (generate-sparql '(select * (?s ?p ?o))
                           nil))
  (should match-except-ws "
SELECT * { ?S ?P ?O1 , ?O2 . }"
          (generate-sparql '(select * (?s ?p (?o1
                                              ?o2)))
                           nil))
  (should match-except-ws "
SELECT *
  { ?S1 ?P1 ?O1 ;
        ?P2 ?O2 ,
            ?O3 .
    ?S4 ?P4 ?O4 . }"
          (generate-sparql '(select * ((?s1 (?p1 ?o1
                                             ?p2 (?o2
                                                  ?o3)))
                                       (?s4 ?p4 ?o4)))
                           nil))
  (should match-except-ws "
SELECT ?X
WHERE { ?X <http://www.w3.org/2001/vcard-rdf/3.0#FN>  \"John Smith\" . }"
          (generate-sparql
           '(select ?x
             where (?x |<http://www.w3.org/2001/vcard-rdf/3.0#FN>| "John Smith"))
           nil))
  (should match-except-ws "
SELECT ?GIVENNAME
WHERE
  { ?Y <http://www.w3.org/2001/vcard-rdf/3.0#Family> \"Smith\" .
    ?Y <http://www.w3.org/2001/vcard-rdf/3.0#Given>  ?GIVENNAME .
  }"
          (generate-sparql
           '(select ?givenName
             where ((?y |<http://www.w3.org/2001/vcard-rdf/3.0#Family>| "Smith")
                    (?y |<http://www.w3.org/2001/vcard-rdf/3.0#Given>| ?givenName)))
           nil))
  (should match-except-ws "
SELECT * { ?S ?P ?O . }
ORDER BY DESC(?S)"
          (generate-sparql '(select * (?s ?p ?o)
                             order by (:desc ?s))
                           nil))
  (should match-except-ws "
SELECT * { ?S ?P ?O . }
ORDER BY x:func(?X)"
          (generate-sparql '(select * (?s ?p ?o)
                             order by (:|x:func| ?x))
                           nil)))

(deftest custom ()
  (should match-except-ws "
PREFIX vCard: <http://www.w3.org/2001/vcard-rdf/3.0#>
SELECT ?GIVENNAME
WHERE
 { ?Y vcard:Family \"Smith\" .
   ?Y vcard:Given  ?GIVENNAME .
 }"
          (generate-sparql '((:prefix |vCard| |<http://www.w3.org/2001/vcard-rdf/3.0#>|)
                             select ?givenName
                             where ((?y |vcard:Family| "Smith")
                                    (?y |vcard:Given| ?givenName)))
                           nil))
  (should match-except-ws "
SELECT ?G
WHERE
{ ?Y vcard:Given ?G .
  FILTER(regex(?G, \"r\", \"i\")) }"
          (generate-sparql '(select ?g
                             where ((?y |vcard:Given| ?g)
                                    (:filter (:regex ?g "r" "i"))))
                           nil))
  (should match-except-ws "
SELECT ?RESOURCE
WHERE
  {
    ?RESOURCE info:age ?AGE .
    FILTER (?AGE >= 24)
  }"
          (generate-sparql '(select ?resource
                             where ((?resource |info:age| ?age)
                                    (:filter (:>= ?age 24))))
                           nil))
  (should match-except-ws "
SELECT ?NAME ?AGE
WHERE
{
    ?PERSON vcard:FN ?NAME .
    OPTIONAL { ?PERSON info:age ?AGE . }
}"
          (generate-sparql '(select ?name ?age
                             where ((?person |vcard:FN| ?name)
                                    (:optional (?person |info:age| ?age))))
                           nil))
  (should match-except-ws "
SELECT ?NAME ?AGE
WHERE
{
    ?PERSON vcard:FN ?NAME .
    OPTIONAL { ?PERSON info:age ?AGE .
               FILTER ( ?AGE > 24 ) }
}"
          (generate-sparql '(select ?name ?age
                             where ((?person |vcard:FN| ?name)
                                    (:optional ((?person |info:age| ?age)
                                                (:filter (:> ?age 24))))))
                           nil))
  (should match-except-ws "
SELECT ?NAME ?AGE
WHERE
{
    ?PERSON vcard:FN ?NAME .
    OPTIONAL { ?PERSON info:age ?AGE . }
    FILTER ( !bound(?AGE) || (?AGE > 24) )
}"
          (generate-sparql '(select ?name ?age
                             where ((?person |vcard:FN| ?name)
                                    (:optional (?person |info:age| ?age))
                                    (:filter (:\|\| (:! (:bound ?age))
                                                    (:> ?age 24)))))
                           nil))
  (should match-except-ws "
SELECT ?NAME
WHERE
{
  ?X a foaf:Person .
  OPTIONAL { ?X foaf:name ?NAME . }
  OPTIONAL { ?X vCard:FN  ?NAME . }
}"
          (generate-sparql '(select ?name
                             where ((?x |a| |foaf:Person|)
                                    (:optional (?x |foaf:name| ?name))
                                    (:optional (?x |vCard:FN| ?name))))
                           nil))
  (should match-except-ws "
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX vCard: <http://www.w3.org/2001/vcard-rdf/3.0#>

SELECT ?NAME
WHERE
{
   { [] foaf:name ?NAME . } UNION { [] vCard:FN ?NAME . }
}"
          (generate-sparql '((:prefix |foaf| |<http://xmlns.com/foaf/0.1/>|)
                             (:prefix |vCard| |<http://www.w3.org/2001/vcard-rdf/3.0#>|)
                             select ?name
                             where (:union ([] |foaf:name| ?name)
                                           ([] |vCard:FN| ?name)))
                           nil))
  (should match-except-ws "
SELECT ?NAME
WHERE
{
  [] ?P ?NAME .
  FILTER ( (?P = foaf:name) || (?P = vCard:FN) )
}"
          (generate-sparql '(select ?name
                             where (([] ?p ?name)
                                    (:filter (:\|\| (:= ?p |foaf:name|)
                                                    (:= ?p |vCard:FN|)))))
                             nil))
  (should match-except-ws "
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX dc: <http://purl.org/dc/elements/1.1/>
PREFIX : <.>

SELECT *
{ ?S ?P ?O . }"
          (generate-sparql '((:prefix |xsd| |<http://www.w3.org/2001/XMLSchema#>|)
                             (:prefix |dc| |<http://purl.org/dc/elements/1.1/>|)
                             (:prefix || |<.>|)
                             select * (?s ?p ?o))
                           nil))
  (should match-except-ws "
SELECT *
{
    { ?S ?P ?O . }
    UNION
    { GRAPH ?G { ?S ?P ?O . } }
}"
          (generate-sparql '(select * (:union (?s ?p ?o)
                                              (:graph ?g (?s ?p ?o))))
                           nil))
  (should match-except-ws "
SELECT ?TITLE
{
  GRAPH :ds-ng-2.ttl
    { ?B dc:title ?TITLE . }
}"
          (generate-sparql '(select ?title (:graph |:ds-ng-2.ttl|
                                            (?b |dc:title| ?title)))
                           nil))
  (should match-except-ws "
SELECT ?DATE ?TITLE
{
  ?G dc:date ?DATE . FILTER (?DATE > \"2005-08-01T00:00:00Z\"^^xsd:dateTime )
  GRAPH ?G { ?B dc:title ?TITLE . }
}"
          (generate-sparql '(select ?date ?title
                             ((?g |dc:date| ?date)
                              (:filter (:> ?date (:|^| "2005-08-01T00:00:00Z"
                                                  |xsd:dateTime|)))
                              (:graph ?g (?b |dc:title| ?title))))
                           nil))
  (should match-except-ws "
SELECT ?G { ?G dc:title \"Foo\"@en . }"
          (generate-sparql '(select ?g (?g |dc:title| (:@ "Foo" "en")))
                           nil))
  (should match-except-ws "
SELECT *
FROM       <ds-dft.ttl>
FROM NAMED <ds-ng-1.ttl>
FROM NAMED <ds-ng-2.ttl>
{
   { ?S ?P ?O . } UNION { GRAPH ?G { ?S ?P ?O . } }
}"
          (generate-sparql '(select *
                             from |<ds-dft.ttl>|
                             from named |<ds-ng-1.ttl>|
                             from named |<ds-ng-2.ttl>|
                             (:union (?s ?p ?o)
                                     (:graph ?g (?s ?p ?o))))
                           nil))
  (should match-except-ws "
SELECT  ?TITLE ?PRICE
FROM <urn:sparql:bind:tests>
  {
    ?X ns:price ?P ;
       ns:discount ?DISCOUNT ;
       <http://purl.org/dc/elements/1.1/title> ?TITLE . 
    BIND (?P*(1-?DISCOUNT) AS ?PRICE)
    FILTER( ?PRICE < 20 )
  }"
          (generate-sparql '(select ?title ?price
                             from |<urn:sparql:bind:tests>|
                             ((?x (|ns:price| ?p
                                   |ns:discount| ?discount
                                   |<http://purl.org/dc/elements/1.1/title>| ?title))
                              (:bind (:* ?p (:- 1 ?discount)) ?price)
                              (:filter (:< ?price 20))))
                           nil)))
