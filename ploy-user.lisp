(uiop:define-package #:ploy/ploy-user
  (:use)
  (:nicknames #:ploy-user)
  (:export
   #:|let| #:|scope| #:|fn| #:|macro| #:|macrolet| #:|if|

   #:|exit|

   #:|backquote| #:|comma| #:|quote|

   #:+

   #:|fixnum| #:|boolean| #:|never| #:|list| #:|forall|

   #:|true| #:|false|

   #:|nil| #:|cons|))
