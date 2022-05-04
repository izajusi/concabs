(define gold-num 5972304273877744135569338397692020533504)
(define signature 143676221783307728140118556730532825709962359695147398872633033728948225540940112091576952965868445265161373616153020167902900930324840824269164789456142215776895016041636987254848119449940440885630)
(define modulus 671629488048603400615365258174985654900765971941961654084193604750896012182890124354255484422321487634816640987992317596893099956961956383454333339584850276505584537663630293912940840460009374858969)
(define signing-exponent 447752992032402267076910172116657103267177314627974436056129069833930674788593416236170322948214322483305175278012793102392215895931470577163544613600143471679799876664686423606429437389098641670667)

(define mod-expt
  (lambda (base exponent modulus)
    (define mod*
      (lambda (x y)
        (remainder (* x y) modulus)))
    (if (= exponent 0)
        1
        (if (even? exponent)
            (let ((x (mod-expt base (/ exponent 2) modulus)))
              (mod* x x))
            (mod* (mod-expt base (- exponent 1) modulus)
                  base)))))

(define mod-expt-v2
  (lambda (base exponent modulus)
    (define mod*
      (lambda (x y)
        (remainder (* x y) modulus)))

    (define iter
      (lambda (x e)
        (if (= e 1)
            x
            (if (even? e)
                (iter (mod* x x) (/ e 2))
                (mod* x (iter x (- e 1)))))))

    (if (= exponent 0)
        1
        (iter base exponent))))

(mod-expt signature 3 modulus)
(mod-expt-v2 signature 3 modulus)

(mod-expt-v2 gold-num signing-exponent modulus)
