# cl-apertium-stream
### _Vee Satayamas <5ssgdxltv@relay.firefox.com>_ 

cl-apertium-stream written in Common Lisp

## Example

```
* (ql:quickload :cl-apertium-stream)
To load "cl-apertium-stream":
  Load 1 ASDF system:
    cl-apertium-stream
; Loading "cl-apertium-stream"
..
(:CL-APERTIUM-STREAM)

* (defvar *parsed-lexical-units* (cl-apertium-stream:parse-stream "^prpers<prn><subj><p1><mf><sg>/ฉัน<prn><pers>$ ^eat<vblex><pres>/กิน<vblex><pres>$ ^rice<n><sg>/ข้าว<n><sg>$"))
*PARSED-LEXICAL-UNITS*

* *parsed-lexical-units*
((:LEXICAL-UNIT
  ((:FLAG) (:LING-FORM . "prpers") (:INVARIABLE-PART)
   (:TAGS "prn" "subj" "p1" "mf" "sg") (:INVARIABLE-PART))
  ((:FLAG) (:LING-FORM . "ฉัน") (:INVARIABLE-PART) (:TAGS "prn" "pers")
   (:INVARIABLE-PART)))
 (:UNPARSED . " ")
 (:LEXICAL-UNIT
  ((:FLAG) (:LING-FORM . "eat") (:INVARIABLE-PART) (:TAGS "vblex" "pres")
   (:INVARIABLE-PART))
  ((:FLAG) (:LING-FORM . "กิน") (:INVARIABLE-PART) (:TAGS "vblex" "pres")
   (:INVARIABLE-PART)))
 (:UNPARSED . " ")
 (:LEXICAL-UNIT
  ((:FLAG) (:LING-FORM . "rice") (:INVARIABLE-PART) (:TAGS "n" "sg")
   (:INVARIABLE-PART))
  ((:FLAG) (:LING-FORM . "ข้าว") (:INVARIABLE-PART) (:TAGS "n" "sg")
   (:INVARIABLE-PART))))

* (cl-apertium-stream:to-stream *parsed-lexical-units*)
"^prpers<prn><subj><p1><mf><sg>/ฉัน<prn><pers>$ ^eat<vblex><pres>/กิน<vblex><pres>$ ^rice<n><sg>/ข้าว<n><sg>$"
```

## License

Apache-2.0
