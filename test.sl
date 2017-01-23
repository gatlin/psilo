(def make-person (\ (name age) (\ (f) (f name age))))
(def gatlin (make-person "gatlin" 28))
(def get-age (\ (p) (p (\ (name age) age))))
(def main (\ () (get-age gatlin)))