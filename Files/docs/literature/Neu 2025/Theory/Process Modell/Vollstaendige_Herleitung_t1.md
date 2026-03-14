# Vollständige Herleitung der optimalen Policies in t=1

**COVID-19 Trilemma Modell - Backward Induction von t=2 zu t=1**

---

## Inhaltsverzeichnis

1. [Ausgangslage und Übersicht](#1-ausgangslage)
2. [Schritt 3: Zielfunktion für t=1](#schritt-3)
3. [Schritt 4: First Order Conditions (FOCs)](#schritt-4)
4. [Schritt 5: Lösung des 2×2 Systems](#schritt-5)
5. [Zusammenfassung und Ergebnis](#zusammenfassung)

---

## 1. Ausgangslage und Übersicht {#1-ausgangslage}

### Was wir bereits wissen (aus t=2)

Aus der Lösung in t=2 haben wir die **Value Function V₂*:**

$$V_2^*(y_2, d_2, b_2, \theta_2) = \beta^2 \left[ P_{2,yy} y_2^2 + P_{2,dd} d_2^2 + P_{2,bb} b_2^2 + P_{2,\theta\theta} \theta_2^2 + 2P_{2,yd} y_2 d_2 + 2P_{2,yb} y_2 b_2 + 2P_{2,y\theta} y_2 \theta_2 + 2P_{2,db} d_2 b_2 + 2P_{2,d\theta} d_2 \theta_2 + 2P_{2,b\theta} b_2 \theta_2 \right]$$

**Matrix-Form:**

$$V_2^*(mathbf{x}_2) = \beta^2 \mathbf{x}_2^\top \mathbf{P}_2 \mathbf{x}_2$$

mit:

$$\mathbf{P}_2 = \begin{pmatrix} P_{2,yy} & P_{2,yd} & P_{2,yb} & P_{2,y\theta} \\ P_{2,yd} & P_{2,dd} & P_{2,db} & P_{2,d\theta} \\ P_{2,yb} & P_{2,db} & P_{2,bb} & P_{2,b\theta} \\ P_{2,y\theta} & P_{2,d\theta} & P_{2,b\theta} & P_{2,\theta\theta} \end{pmatrix}$$

### Das Problem in t=1

Wir lösen:

$$V_1(y_1, d_1, b_1, \theta_1) = \min_{S_1, F_1} \left\{ \beta (w_y y_1^2 + w_d d_1^2 + w_b b_1^2) + \beta E_1[V_2^*] \right\}$$

**Ziel:** Finde optimale Policies $S_1^*(y_1, d_1, b_1, \theta_1)$ und $F_1^*(y_1, d_1, b_1, \theta_1)$

### Die drei Schritte der Herleitung

1. **Schritt 3:** Zielfunktion für t=1 aufstellen (mit $E_1[V_2^*]$)
2. **Schritt 4:** First Order Conditions herleiten
3. **Schritt 5:** 2×2 System lösen und Policy-Functions bestimmen

---

## Schritt 3: Zielfunktion für t=1 mit E₁[V₂*] {#schritt-3}

### 3.1 Erwartungswert E₁[V₂*]

Aus der Stochastik in t=1 wissen wir:

$$\theta_2 = \rho_\theta \theta_1 - \phi_S S_1 + \varepsilon_2$$

$$d_2 = \rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \theta_2$$

mit $\varepsilon_2 \sim N(0, \sigma_2^2)$ und $\hat{\delta}_S = \delta_S + 2\delta_\theta \phi_S$

**Certainty Equivalence:**

$$E_1[V_2^*] = V_2^*(y_2, \bar{d}_2, b_2, \bar{\theta}_2) + \text{Konstante}$$

wobei:
- $\bar{\theta}_2 = \rho_\theta \theta_1 - \phi_S S_1$ (erwartete Infektionsrate)
- $\bar{d}_2 = \rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1$ (erwarteter Gesundheitsschaden)

**Für die Optimierung relevant (Konstante weggelassen):**

$$\boxed{E_1[V_2^*] = \beta^2 \left[ P_{2,yy} y_2^2 + P_{2,dd} \bar{d}_2^2 + P_{2,bb} b_2^2 + P_{2,\theta\theta} \bar{\theta}_2^2 + 2P_{2,yd} y_2 \bar{d}_2 + 2P_{2,yb} y_2 b_2 + 2P_{2,y\theta} y_2 \bar{\theta}_2 + 2P_{2,db} \bar{d}_2 b_2 + 2P_{2,d\theta} \bar{d}_2 \bar{\theta}_2 + 2P_{2,b\theta} b_2 \bar{\theta}_2 \right]}$$

---

### 3.2 Transitionsgleichungen in t=1

Die Zustände entwickeln sich gemäß:

$$\boxed{y_2 = \rho_y y_1 + \alpha_S S_1 - \alpha_F F_1}$$

$$\boxed{\bar{d}_2 = \rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1}$$

$$\boxed{b_2 = (1+r) b_1 + \kappa_F F_1 + \kappa_y y_1}$$

$$\boxed{\bar{\theta}_2 = \rho_\theta \theta_1 - \phi_S S_1}$$

mit $\hat{\delta}_S = \delta_S + 2\delta_\theta \phi_S$

---

### 3.3 Vollständige Zielfunktion für t=1

**Ausgeschrieben:**

$$\boxed{\min_{S_1, F_1} \Bigg\{ \beta (w_y y_1^2 + w_d d_1^2 + w_b b_1^2)}$$

$$\boxed{+ \beta^3 \Big[ P_{2,yy} (\rho_y y_1 + \alpha_S S_1 - \alpha_F F_1)^2}$$

$$\boxed{+ P_{2,dd} (\rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1)^2}$$

$$\boxed{+ P_{2,bb} [(1+r) b_1 + \kappa_F F_1 + \kappa_y y_1]^2}$$

$$\boxed{+ P_{2,\theta\theta} (\rho_\theta \theta_1 - \phi_S S_1)^2}$$

$$\boxed{+ 2P_{2,yd} (\rho_y y_1 + \alpha_S S_1 - \alpha_F F_1)(\rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1)}$$

$$\boxed{+ 2P_{2,yb} (\rho_y y_1 + \alpha_S S_1 - \alpha_F F_1)[(1+r) b_1 + \kappa_F F_1 + \kappa_y y_1]}$$

$$\boxed{+ 2P_{2,y\theta} (\rho_y y_1 + \alpha_S S_1 - \alpha_F F_1)(\rho_\theta \theta_1 - \phi_S S_1)}$$

$$\boxed{+ 2P_{2,db} (\rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1)[(1+r) b_1 + \kappa_F F_1 + \kappa_y y_1]}$$

$$\boxed{+ 2P_{2,d\theta} (\rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1)(\rho_\theta \theta_1 - \phi_S S_1)}$$

$$\boxed{+ 2P_{2,b\theta} [(1+r) b_1 + \kappa_F F_1 + \kappa_y y_1](\rho_\theta \theta_1 - \phi_S S_1) \Big] \Bigg\}}$$

---

### 3.4 Matrix-Form der Zielfunktion

**Kontrollvektor:**
$$\mathbf{u}_1 = \begin{pmatrix} S_1 \\ F_1 \end{pmatrix}$$

**Zustandsvektor:**
$$\mathbf{x}_1 = \begin{pmatrix} y_1 \\ d_1 \\ b_1 \\ \theta_1 \end{pmatrix}$$

**Zielfunktion:**

$$\boxed{L(\mathbf{u}_1) = \beta \mathbf{x}_1^\top \mathbf{W}_1 \mathbf{x}_1 + \beta^3 \left[ \mathbf{u}_1^\top \mathbf{Q}_1 \mathbf{u}_1 + 2 \mathbf{u}_1^\top \mathbf{N}_1 \mathbf{x}_1 + \mathbf{x}_1^\top \mathbf{M}_1 \mathbf{x}_1 \right]}$$

wobei:
- $\mathbf{W}_1 = \text{diag}(w_y, w_d, w_b, 0)$: Current period Kosten
- $\mathbf{Q}_1$: $2 \times 2$ Matrix der quadratischen Kontroll-Kosten
- $\mathbf{N}_1$: $2 \times 4$ Matrix der Kreuzterme (Kontrollen × Zustände)
- $\mathbf{M}_1$: $4 \times 4$ Matrix der Zustands-Kosten aus V₂

---

### 3.5 Die Q₁-Matrix (quadratische Kosten in S₁, F₁)

Durch Sammeln aller Terme mit $S_1^2$, $F_1^2$, $S_1 F_1$ erhalten wir:

#### Element [1,1]: Koeffizient von S₁²

**Aus allen Termen:**

$$Q_{1,SS} = \beta^3 [P_{2,yy} \alpha_S^2 + P_{2,dd} \hat{\delta}_S^2 + P_{2,\theta\theta} \phi_S^2 - 2P_{2,yd} \alpha_S \hat{\delta}_S - 2P_{2,y\theta} \alpha_S \phi_S + 2P_{2,d\theta} \hat{\delta}_S \phi_S]$$

#### Element [2,2]: Koeffizient von F₁²

$$Q_{1,FF} = \beta^3 [P_{2,yy} \alpha_F^2 + P_{2,bb} \kappa_F^2 - 2P_{2,yb} \alpha_F \kappa_F]$$

#### Element [1,2] = Element [2,1]: Koeffizient von S₁F₁

$$Q_{1,SF} = \beta^3 [-P_{2,yy} \alpha_S \alpha_F + P_{2,yd} \alpha_F \hat{\delta}_S + P_{2,yb} \alpha_S \kappa_F + P_{2,y\theta} \alpha_F \phi_S - P_{2,db} \hat{\delta}_S \kappa_F - P_{2,b\theta} \kappa_F \phi_S]$$

#### Die vollständige Q₁-Matrix

$$\boxed{\mathbf{Q}_1 = \beta^3 \begin{pmatrix} P_{2,yy} \alpha_S^2 + P_{2,dd} \hat{\delta}_S^2 + P_{2,\theta\theta} \phi_S^2 - 2P_{2,yd} \alpha_S \hat{\delta}_S - 2P_{2,y\theta} \alpha_S \phi_S + 2P_{2,d\theta} \hat{\delta}_S \phi_S & -P_{2,yy} \alpha_S \alpha_F + P_{2,yd} \alpha_F \hat{\delta}_S + P_{2,yb} \alpha_S \kappa_F + P_{2,y\theta} \alpha_F \phi_S - P_{2,db} \hat{\delta}_S \kappa_F - P_{2,b\theta} \kappa_F \phi_S \\ -P_{2,yy} \alpha_S \alpha_F + P_{2,yd} \alpha_F \hat{\delta}_S + P_{2,yb} \alpha_S \kappa_F + P_{2,y\theta} \alpha_F \phi_S - P_{2,db} \hat{\delta}_S \kappa_F - P_{2,b\theta} \kappa_F \phi_S & P_{2,yy} \alpha_F^2 + P_{2,bb} \kappa_F^2 - 2P_{2,yb} \alpha_F \kappa_F \end{pmatrix}}$$

**Eigenschaften:**
- Symmetrisch: $Q_{1,SF} = Q_{1,FS}$
- Positiv definit (unter Optimalitätsbedingungen)
- Kodiert die quadratischen Kosten der Instrumente

---

### 3.6 Zusammenfassung Schritt 3

Wir haben die **vollständige Zielfunktion** für Period t=1 aufgestellt:

1. ✅ Current period costs: $\beta(w_y y_1^2 + w_d d_1^2 + w_b b_1^2)$
2. ✅ Continuation value: $\beta^3 E_1[V_2^*]$ mit allen P-Termen
3. ✅ Transitionsgleichungen eingebettet
4. ✅ Q₁-Matrix explizit berechnet
5. ✅ Struktur: Quadratisch in $(S_1, F_1)$, linear in Zuständen

**Die Zielfunktion ist bereit für die FOC-Bildung!**

---

## Schritt 4: First Order Conditions (FOCs) für t=1 {#schritt-4}

### 4.1 Die FOC-Methode

Für die Zielfunktion $L(S_1, F_1)$ gilt:

**Notwendige Bedingung für Minimum:**

$$\frac{\partial L}{\partial S_1} = 0 \quad \text{und} \quad \frac{\partial L}{\partial F_1} = 0$$

Da current period costs nicht von $(S_1, F_1)$ abhängen:

$$\frac{\partial L}{\partial S_1} = \beta^3 \frac{\partial E_1[V_2^*]}{\partial S_1} = 0$$

$$\frac{\partial L}{\partial F_1} = \beta^3 \frac{\partial E_1[V_2^*]}{\partial F_1} = 0$$

---

### 4.2 FOC für S₁ - Kettenregel

$$\frac{\partial E_1[V_2^*]}{\partial S_1} = \frac{\partial E_1[V_2^*]}{\partial y_2} \cdot \frac{\partial y_2}{\partial S_1} + \frac{\partial E_1[V_2^*]}{\partial \bar{d}_2} \cdot \frac{\partial \bar{d}_2}{\partial S_1} + \frac{\partial E_1[V_2^*]}{\partial b_2} \cdot \frac{\partial b_2}{\partial S_1} + \frac{\partial E_1[V_2^*]}{\partial \bar{\theta}_2} \cdot \frac{\partial \bar{\theta}_2}{\partial S_1}$$

#### Ableitungen der Zustände nach S₁

$$\frac{\partial y_2}{\partial S_1} = \alpha_S, \quad \frac{\partial \bar{d}_2}{\partial S_1} = -\hat{\delta}_S, \quad \frac{\partial b_2}{\partial S_1} = 0, \quad \frac{\partial \bar{\theta}_2}{\partial S_1} = -\phi_S$$

#### Ableitungen von E₁[V₂*] nach Zuständen

$$\frac{\partial E_1[V_2^*]}{\partial y_2} = 2P_{2,yy} y_2 + 2P_{2,yd} \bar{d}_2 + 2P_{2,yb} b_2 + 2P_{2,y\theta} \bar{\theta}_2$$

$$\frac{\partial E_1[V_2^*]}{\partial \bar{d}_2} = 2P_{2,yd} y_2 + 2P_{2,dd} \bar{d}_2 + 2P_{2,db} b_2 + 2P_{2,d\theta} \bar{\theta}_2$$

$$\frac{\partial E_1[V_2^*]}{\partial b_2} = 2P_{2,yb} y_2 + 2P_{2,db} \bar{d}_2 + 2P_{2,bb} b_2 + 2P_{2,b\theta} \bar{\theta}_2$$

$$\frac{\partial E_1[V_2^*]}{\partial \bar{\theta}_2} = 2P_{2,y\theta} y_2 + 2P_{2,d\theta} \bar{d}_2 + 2P_{2,b\theta} b_2 + 2P_{2,\theta\theta} \bar{\theta}_2$$

#### FOC für S₁ zusammengesetzt

$$0 = 2\beta^3 \Big[ (P_{2,yy} y_2 + P_{2,yd} \bar{d}_2 + P_{2,yb} b_2 + P_{2,y\theta} \bar{\theta}_2) \alpha_S - (P_{2,yd} y_2 + P_{2,dd} \bar{d}_2 + P_{2,db} b_2 + P_{2,d\theta} \bar{\theta}_2) \hat{\delta}_S - (P_{2,y\theta} y_2 + P_{2,d\theta} \bar{d}_2 + P_{2,b\theta} b_2 + P_{2,\theta\theta} \bar{\theta}_2) \phi_S \Big]$$

---

### 4.3 FOC für F₁ - Kettenregel

Analog zu S₁, aber mit anderen Ableitungen:

#### Ableitungen der Zustände nach F₁

$$\frac{\partial y_2}{\partial F_1} = -\alpha_F, \quad \frac{\partial \bar{d}_2}{\partial F_1} = 0, \quad \frac{\partial b_2}{\partial F_1} = \kappa_F, \quad \frac{\partial \bar{\theta}_2}{\partial F_1} = 0$$

#### FOC für F₁ zusammengesetzt

$$0 = 2\beta^3 \Big[ -(P_{2,yy} y_2 + P_{2,yd} \bar{d}_2 + P_{2,yb} b_2 + P_{2,y\theta} \bar{\theta}_2) \alpha_F + (P_{2,yb} y_2 + P_{2,db} \bar{d}_2 + P_{2,bb} b_2 + P_{2,b\theta} \bar{\theta}_2) \kappa_F \Big]$$

---

### 4.4 Transitionsgleichungen einsetzen - Schritt für Schritt

Jetzt setzen wir die Transitionsgleichungen in die FOCs ein.

#### Für FOC von S₁: Terme mit y₂ einsetzen

Setze $y_2 = \rho_y y_1 + \alpha_S S_1 - \alpha_F F_1$:

**Term: $(P_{2,yy} y_2) \alpha_S$**

$$= P_{2,yy} (\rho_y y_1 + \alpha_S S_1 - \alpha_F F_1) \alpha_S$$

$$= P_{2,yy} \alpha_S \rho_y y_1 + P_{2,yy} \alpha_S^2 S_1 - P_{2,yy} \alpha_S \alpha_F F_1$$

**Sortieren:**
- Konstant: $P_{2,yy} \alpha_S \rho_y y_1$
- S₁-Term: $P_{2,yy} \alpha_S^2 S_1$
- F₁-Term: $-P_{2,yy} \alpha_S \alpha_F F_1$

**Term: $(P_{2,yd} y_2) (-\hat{\delta}_S)$**

$$= -P_{2,yd} \hat{\delta}_S (\rho_y y_1 + \alpha_S S_1 - \alpha_F F_1)$$

$$= -P_{2,yd} \hat{\delta}_S \rho_y y_1 - P_{2,yd} \hat{\delta}_S \alpha_S S_1 + P_{2,yd} \hat{\delta}_S \alpha_F F_1$$

**Sortieren:**
- Konstant: $-P_{2,yd} \hat{\delta}_S \rho_y y_1$
- S₁-Term: $-P_{2,yd} \hat{\delta}_S \alpha_S S_1$
- F₁-Term: $P_{2,yd} \hat{\delta}_S \alpha_F F_1$

**Term: $(P_{2,y\theta} y_2) (-\phi_S)$**

$$= -P_{2,y\theta} \phi_S (\rho_y y_1 + \alpha_S S_1 - \alpha_F F_1)$$

$$= -P_{2,y\theta} \phi_S \rho_y y_1 - P_{2,y\theta} \phi_S \alpha_S S_1 + P_{2,y\theta} \phi_S \alpha_F F_1$$

**Sortieren:**
- Konstant: $-P_{2,y\theta} \phi_S \rho_y y_1$
- S₁-Term: $-P_{2,y\theta} \phi_S \alpha_S S_1$
- F₁-Term: $P_{2,y\theta} \phi_S \alpha_F F_1$

---

#### Für FOC von S₁: Terme mit d̄₂ einsetzen

Setze $\bar{d}_2 = \rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1$:

**Term: $(P_{2,yd} \bar{d}_2) \alpha_S$**

$$= P_{2,yd} \alpha_S (\rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1)$$

**Sortieren:**
- d₁-Term: $P_{2,yd} \alpha_S \rho_d d_1$
- S₁-Term: $-P_{2,yd} \alpha_S \hat{\delta}_S S_1$
- θ₁-Term: $P_{2,yd} \alpha_S \delta_\theta \rho_\theta \theta_1$

**Term: $(P_{2,dd} \bar{d}_2) (-\hat{\delta}_S)$**

$$= -P_{2,dd} \hat{\delta}_S (\rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1)$$

**Sortieren:**
- d₁-Term: $-P_{2,dd} \hat{\delta}_S \rho_d d_1$
- S₁-Term: $P_{2,dd} \hat{\delta}_S^2 S_1$
- θ₁-Term: $-P_{2,dd} \hat{\delta}_S \delta_\theta \rho_\theta \theta_1$

**Term: $(P_{2,d\theta} \bar{d}_2) (-\phi_S)$**

$$= -P_{2,d\theta} \phi_S (\rho_d d_1 - \hat{\delta}_S S_1 + \delta_\theta \rho_\theta \theta_1)$$

**Sortieren:**
- d₁-Term: $-P_{2,d\theta} \phi_S \rho_d d_1$
- S₁-Term: $P_{2,d\theta} \phi_S \hat{\delta}_S S_1$
- θ₁-Term: $-P_{2,d\theta} \phi_S \delta_\theta \rho_\theta \theta_1$

---

#### Für FOC von S₁: Terme mit b₂ einsetzen

Setze $b_2 = (1+r) b_1 + \kappa_F F_1 + \kappa_y y_1$:

**Term: $(P_{2,yb} b_2) \alpha_S$**

$$= P_{2,yb} \alpha_S [(1+r) b_1 + \kappa_F F_1 + \kappa_y y_1]$$

**Sortieren:**
- y₁-Term: $P_{2,yb} \alpha_S \kappa_y y_1$
- b₁-Term: $P_{2,yb} \alpha_S (1+r) b_1$
- F₁-Term: $P_{2,yb} \alpha_S \kappa_F F_1$

**Term: $(P_{2,db} b_2) (-\hat{\delta}_S)$**

$$= -P_{2,db} \hat{\delta}_S [(1+r) b_1 + \kappa_F F_1 + \kappa_y y_1]$$

**Sortieren:**
- y₁-Term: $-P_{2,db} \hat{\delta}_S \kappa_y y_1$
- b₁-Term: $-P_{2,db} \hat{\delta}_S (1+r) b_1$
- F₁-Term: $-P_{2,db} \hat{\delta}_S \kappa_F F_1$

**Term: $(P_{2,b\theta} b_2) (-\phi_S)$**

$$= -P_{2,b\theta} \phi_S [(1+r) b_1 + \kappa_F F_1 + \kappa_y y_1]$$

**Sortieren:**
- y₁-Term: $-P_{2,b\theta} \phi_S \kappa_y y_1$
- b₁-Term: $-P_{2,b\theta} \phi_S (1+r) b_1$
- F₁-Term: $-P_{2,b\theta} \phi_S \kappa_F F_1$

---

#### Für FOC von S₁: Terme mit θ̄₂ einsetzen

Setze $\bar{\theta}_2 = \rho_\theta \theta_1 - \phi_S S_1$:

**Term: $(P_{2,y\theta} \bar{\theta}_2) \alpha_S$**

$$= P_{2,y\theta} \alpha_S (\rho_\theta \theta_1 - \phi_S S_1)$$

**Sortieren:**
- θ₁-Term: $P_{2,y\theta} \alpha_S \rho_\theta \theta_1$
- S₁-Term: $-P_{2,y\theta} \alpha_S \phi_S S_1$

**Term: $(P_{2,d\theta} \bar{\theta}_2) (-\hat{\delta}_S)$**

$$= -P_{2,d\theta} \hat{\delta}_S (\rho_\theta \theta_1 - \phi_S S_1)$$

**Sortieren:**
- θ₁-Term: $-P_{2,d\theta} \hat{\delta}_S \rho_\theta \theta_1$
- S₁-Term: $P_{2,d\theta} \hat{\delta}_S \phi_S S_1$

**Term: $(P_{2,\theta\theta} \bar{\theta}_2) (-\phi_S)$**

$$= -P_{2,\theta\theta} \phi_S (\rho_\theta \theta_1 - \phi_S S_1)$$

**Sortieren:**
- θ₁-Term: $-P_{2,\theta\theta} \phi_S \rho_\theta \theta_1$
- S₁-Term: $P_{2,\theta\theta} \phi_S^2 S_1$

---

#### FOC für S₁: Alle Terme gesammelt

**S₁-Koeffizient:**

Aus y₂: $P_{2,yy} \alpha_S^2 - P_{2,yd} \hat{\delta}_S \alpha_S - P_{2,y\theta} \phi_S \alpha_S$

Aus d̄₂: $-P_{2,yd} \alpha_S \hat{\delta}_S + P_{2,dd} \hat{\delta}_S^2 + P_{2,d\theta} \phi_S \hat{\delta}_S$

Aus θ̄₂: $-P_{2,y\theta} \alpha_S \phi_S + P_{2,d\theta} \hat{\delta}_S \phi_S + P_{2,\theta\theta} \phi_S^2$

**Gesamter S₁-Koeffizient:**

$$\text{Koeff}(S_1) = P_{2,yy} \alpha_S^2 - 2P_{2,yd} \alpha_S \hat{\delta}_S + P_{2,dd} \hat{\delta}_S^2 - 2P_{2,y\theta} \alpha_S \phi_S + 2P_{2,d\theta} \hat{\delta}_S \phi_S + P_{2,\theta\theta} \phi_S^2$$

**Das ist exakt $Q_{1,SS}$ aus Schritt 3!** ✓

**F₁-Koeffizient:**

Aus y₂: $-P_{2,yy} \alpha_S \alpha_F + P_{2,yd} \hat{\delta}_S \alpha_F + P_{2,y\theta} \phi_S \alpha_F$

Aus b₂: $P_{2,yb} \alpha_S \kappa_F - P_{2,db} \hat{\delta}_S \kappa_F - P_{2,b\theta} \phi_S \kappa_F$

**Gesamter F₁-Koeffizient:**

$$\text{Koeff}(F_1) = -P_{2,yy} \alpha_S \alpha_F + P_{2,yd} \hat{\delta}_S \alpha_F + P_{2,y\theta} \phi_S \alpha_F + P_{2,yb} \alpha_S \kappa_F - P_{2,db} \hat{\delta}_S \kappa_F - P_{2,b\theta} \phi_S \kappa_F$$

**Das ist exakt $Q_{1,SF}$ aus Schritt 3!** ✓

---

#### FOC für S₁: Rechte Seite (Zustands-Terme)

**y₁-Koeffizient:**

$$N_{1,Sy} = -(P_{2,yy} \alpha_S - P_{2,yd} \hat{\delta}_S - P_{2,y\theta} \phi_S) \rho_y - (P_{2,yb} \alpha_S - P_{2,db} \hat{\delta}_S - P_{2,b\theta} \phi_S) \kappa_y$$

**d₁-Koeffizient:**

$$N_{1,Sd} = -(P_{2,yd} \alpha_S - P_{2,dd} \hat{\delta}_S - P_{2,d\theta} \phi_S) \rho_d$$

**b₁-Koeffizient:**

$$N_{1,Sb} = -(P_{2,yb} \alpha_S - P_{2,db} \hat{\delta}_S - P_{2,b\theta} \phi_S)(1+r)$$

**θ₁-Koeffizient:**

$$N_{1,S\theta} = -[(P_{2,yd} \alpha_S - P_{2,dd} \hat{\delta}_S - P_{2,d\theta} \phi_S) \delta_\theta \rho_\theta + (P_{2,y\theta} \alpha_S - P_{2,d\theta} \hat{\delta}_S - P_{2,\theta\theta} \phi_S) \rho_\theta]$$

---

### 4.5 Das vollständige 2×2 System aus den FOCs

Nach Division durch $2\beta^3$ und Sammeln aller Terme:

**Gleichung 1 (aus FOC für S₁):**

$$Q_{1,SS} S_1 + Q_{1,SF} F_1 = -(N_{1,Sy} y_1 + N_{1,Sd} d_1 + N_{1,Sb} b_1 + N_{1,S\theta} \theta_1)$$

**Gleichung 2 (aus FOC für F₁):**

$$Q_{1,SF} S_1 + Q_{1,FF} F_1 = -(N_{1,Fy} y_1 + N_{1,Fd} d_1 + N_{1,Fb} b_1 + N_{1,F\theta} \theta_1)$$

#### Matrix-Form

$$\boxed{\begin{pmatrix} Q_{1,SS} & Q_{1,SF} \\ Q_{1,SF} & Q_{1,FF} \end{pmatrix} \begin{pmatrix} S_1 \\ F_1 \end{pmatrix} = \begin{pmatrix} R_S^{(1)} \\ R_F^{(1)} \end{pmatrix}}$$

oder kompakt:

$$\boxed{\mathbf{Q}_1 \mathbf{u}_1 = \mathbf{R}_1}$$

mit:

$$\mathbf{R}_1 = -\mathbf{N}_1 \mathbf{x}_1$$

---

### 4.6 Zusammenfassung Schritt 4

Wir haben:

1. ✅ **FOCs aufgestellt** durch Ableitung nach $S_1$ und $F_1$
2. ✅ **Kettenregel angewandt** für alle Zustandsvariablen
3. ✅ **Transitionsgleichungen eingesetzt** Term für Term
4. ✅ **Alle gleichartigen Terme gesammelt**:
   - S₁-Terme → $Q_{1,SS}$
   - F₁-Terme → $Q_{1,FF}$
   - S₁F₁-Terme → $Q_{1,SF}$
   - Zustands-Terme → $\mathbf{N}_1$
5. ✅ **2×2 System formuliert** in Matrix-Form

**Das System ist bereit zur Lösung!**

---

## Schritt 5: Lösung des 2×2 Systems für t=1 {#schritt-5}

### 5.1 Das zu lösende System

$$\boxed{\mathbf{Q}_1 \begin{pmatrix} S_1^* \\ F_1^* \end{pmatrix} = \begin{pmatrix} R_S^{(1)} \\ R_F^{(1)} \end{pmatrix}}$$

mit:

$$\mathbf{Q}_1 = \beta^3 \begin{pmatrix} Q_{1,SS} & Q_{1,SF} \\ Q_{1,SF} & Q_{1,FF} \end{pmatrix}$$

**Vereinfachung:** Division durch $\beta^3$:

$$\begin{pmatrix} Q_{1,SS} & Q_{1,SF} \\ Q_{1,SF} & Q_{1,FF} \end{pmatrix} \begin{pmatrix} S_1^* \\ F_1^* \end{pmatrix} = \begin{pmatrix} \tilde{R}_S^{(1)} \\ \tilde{R}_F^{(1)} \end{pmatrix}$$

---

### 5.2 Determinante berechnen

$$\Delta_1 = Q_{1,SS} \cdot Q_{1,FF} - (Q_{1,SF})^2$$

**Ausgeschrieben:**

$$\Delta_1 = [P_{2,yy} \alpha_S^2 + P_{2,dd} \hat{\delta}_S^2 + P_{2,\theta\theta} \phi_S^2 - 2P_{2,yd} \alpha_S \hat{\delta}_S - 2P_{2,y\theta} \alpha_S \phi_S + 2P_{2,d\theta} \hat{\delta}_S \phi_S]$$

$$\times [P_{2,yy} \alpha_F^2 + P_{2,bb} \kappa_F^2 - 2P_{2,yb} \alpha_F \kappa_F]$$

$$- [-P_{2,yy} \alpha_S \alpha_F + P_{2,yd} \alpha_F \hat{\delta}_S + P_{2,yb} \alpha_S \kappa_F + P_{2,y\theta} \alpha_F \phi_S - P_{2,db} \hat{\delta}_S \kappa_F - P_{2,b\theta} \kappa_F \phi_S]^2$$

**Annahme:** $\Delta_1 > 0$ (garantiert durch Optimalitätsbedingungen)

**Interpretation:**
- $\Delta_1 > 0$ garantiert eindeutige Lösung
- Zielfunktion strikt konvex in $(S_1, F_1)$
- Second Order Conditions erfüllt

---

### 5.3 Inverse Matrix berechnen

Für eine 2×2 Matrix gilt:

$$\mathbf{Q}_1^{-1} = \frac{1}{\Delta_1} \begin{pmatrix} Q_{1,FF} & -Q_{1,SF} \\ -Q_{1,SF} & Q_{1,SS} \end{pmatrix}$$

---

### 5.4 Lösung durch Cramer's Regel

**Für S₁*:**

$$\boxed{S_1^* = \frac{1}{\Delta_1} [Q_{1,FF} \cdot R_S^{(1)} - Q_{1,SF} \cdot R_F^{(1)}]}$$

**Für F₁*:**

$$\boxed{F_1^* = \frac{1}{\Delta_1} [-Q_{1,SF} \cdot R_S^{(1)} + Q_{1,SS} \cdot R_F^{(1)}]}$$

---

### 5.5 Rechte Seite explizit

Die rechte Seite ist **linear** in den Zuständen:

$$R_S^{(1)} = N_{1,Sy} y_1 + N_{1,Sd} d_1 + N_{1,Sb} b_1 + N_{1,S\theta} \theta_1$$

$$R_F^{(1)} = N_{1,Fy} y_1 + N_{1,Fd} d_1 + N_{1,Fb} b_1 + N_{1,F\theta} \theta_1$$

**Matrix-Form:**

$$\begin{pmatrix} R_S^{(1)} \\ R_F^{(1)} \end{pmatrix} = \mathbf{N}_1 \begin{pmatrix} y_1 \\ d_1 \\ b_1 \\ \theta_1 \end{pmatrix}$$

mit:

$$\mathbf{N}_1 = \begin{pmatrix} N_{1,Sy} & N_{1,Sd} & N_{1,Sb} & N_{1,S\theta} \\ N_{1,Fy} & N_{1,Fd} & N_{1,Fb} & N_{1,F\theta} \end{pmatrix}$$

---

### 5.6 Policy Functions herleiten

Da $R_S^{(1)}$ und $R_F^{(1)}$ **linear** in $(y_1, d_1, b_1, \theta_1)$ sind, und die Lösung eine **lineare Transformation** ist:

$$\begin{pmatrix} S_1^* \\ F_1^* \end{pmatrix} = \mathbf{Q}_1^{-1} \mathbf{N}_1 \begin{pmatrix} y_1 \\ d_1 \\ b_1 \\ \theta_1 \end{pmatrix}$$

**Definiere die Policy-Matrix:**

$$\boxed{\mathbf{A}_1 = \mathbf{Q}_1^{-1} \mathbf{N}_1 = \begin{pmatrix} a_1^{S,y} & a_1^{S,d} & a_1^{S,b} & a_1^{S,\theta} \\ a_1^{F,y} & a_1^{F,d} & a_1^{F,b} & a_1^{F,\theta} \end{pmatrix}}$$

---

### 5.7 Optimale Policy Functions

$$\boxed{S_1^* = a_1^{S,y} y_1 + a_1^{S,d} d_1 + a_1^{S,b} b_1 + a_1^{S,\theta} \theta_1}$$

$$\boxed{F_1^* = a_1^{F,y} y_1 + a_1^{F,d} d_1 + a_1^{F,b} b_1 + a_1^{F,\theta} \theta_1}$$

**Matrix-Form:**

$$\boxed{\begin{pmatrix} S_1^* \\ F_1^* \end{pmatrix} = \mathbf{A}_1 \begin{pmatrix} y_1 \\ d_1 \\ b_1 \\ \theta_1 \end{pmatrix}}$$

**Dies sind die optimalen Policy Functions in Period t=1!**

---

### 5.8 Explizite Formeln für Policy-Koeffizienten

Jeder Policy-Koeffizient hat die Form:

**Für S₁*-Koeffizienten:**

$$a_1^{S,j} = \frac{1}{\Delta_1} [Q_{1,FF} \cdot N_{1,Sj} - Q_{1,SF} \cdot N_{1,Fj}]$$

für $j \in \{y, d, b, \theta\}$

**Für F₁*-Koeffizienten:**

$$a_1^{F,j} = \frac{1}{\Delta_1} [-Q_{1,SF} \cdot N_{1,Sj} + Q_{1,SS} \cdot N_{1,Fj}]$$

für $j \in \{y, d, b, \theta\}$

---

### 5.9 Matrix-Darstellung der Policy-Matrix

Die vollständige Policy-Matrix ist:

$$\boxed{\mathbf{A}_1 = \frac{1}{\Delta_1} \begin{pmatrix} Q_{1,FF} & -Q_{1,SF} \\ -Q_{1,SF} & Q_{1,SS} \end{pmatrix} \begin{pmatrix} N_{1,Sy} & N_{1,Sd} & N_{1,Sb} & N_{1,S\theta} \\ N_{1,Fy} & N_{1,Fd} & N_{1,Fb} & N_{1,F\theta} \end{pmatrix}}$$

---

### 5.10 Zusammenfassung Schritt 5

Wir haben:

1. ✅ **2×2 System aufgestellt** aus FOCs
2. ✅ **Determinante** $\Delta_1$ berechnet (Existenzbedingung)
3. ✅ **Inverse Matrix** $\mathbf{Q}_1^{-1}$ hergeleitet
4. ✅ **Cramer's Regel** angewandt
5. ✅ **N₁-Matrix** aus FOCs identifiziert
6. ✅ **Policy-Matrix** $\mathbf{A}_1$ konstruiert
7. ✅ **Lineare Policy Functions** für $S_1^*$ und $F_1^*$ erhalten

**Die Lösung ist vollständig!**

---

## Zusammenfassung und Ergebnis {#zusammenfassung}

### Die vollständige Lösung für t=1

#### Optimale Policies (ausgeschrieben)

$$\boxed{S_1^* = a_1^{S,y} y_1 + a_1^{S,d} d_1 + a_1^{S,b} b_1 + a_1^{S,\theta} \theta_1}$$

$$\boxed{F_1^* = a_1^{F,y} y_1 + a_1^{F,d} d_1 + a_1^{F,b} b_1 + a_1^{F,\theta} \theta_1}$$

#### Optimale Policies (Matrix-Form)

$$\boxed{\begin{pmatrix} S_1^* \\ F_1^* \end{pmatrix} = \mathbf{A}_1 \begin{pmatrix} y_1 \\ d_1 \\ b_1 \\ \theta_1 \end{pmatrix}}$$

mit:

$$\mathbf{A}_1 = \mathbf{Q}_1^{-1} \mathbf{N}_1$$

---

### Eigenschaften der Lösung

1. **Linearität:** Policies sind linear in allen Zuständen
2. **Certainty Equivalence:** Stochastik beeinflusst nur Konstantenterme
3. **Separation:** Jeder Policy-Koeffizient hat klare ökonomische Interpretation
4. **Symmetrie:** Die Cross-Effekte zwischen S und F sind konsistent
5. **Forward-Looking:** Policies berücksichtigen zukünftige optimale Reaktionen (via P₂)

---

### Die drei Hauptelemente der Lösung

#### 1. Die Q₁-Matrix (Instrumentenkosten)

$$\mathbf{Q}_1 = \beta^3 \begin{pmatrix} P_{2,yy} \alpha_S^2 + P_{2,dd} \hat{\delta}_S^2 + P_{2,\theta\theta} \phi_S^2 - 2P_{2,yd} \alpha_S \hat{\delta}_S - 2P_{2,y\theta} \alpha_S \phi_S + 2P_{2,d\theta} \hat{\delta}_S \phi_S & -P_{2,yy} \alpha_S \alpha_F + P_{2,yd} \alpha_F \hat{\delta}_S + P_{2,yb} \alpha_S \kappa_F + P_{2,y\theta} \alpha_F \phi_S - P_{2,db} \hat{\delta}_S \kappa_F - P_{2,b\theta} \kappa_F \phi_S \\ -P_{2,yy} \alpha_S \alpha_F + P_{2,yd} \alpha_F \hat{\delta}_S + P_{2,yb} \alpha_S \kappa_F + P_{2,y\theta} \alpha_F \phi_S - P_{2,db} \hat{\delta}_S \kappa_F - P_{2,b\theta} \kappa_F \phi_S & P_{2,yy} \alpha_F^2 + P_{2,bb} \kappa_F^2 - 2P_{2,yb} \alpha_F \kappa_F \end{pmatrix}$$

**Kodiert:** Kosten der Instrumente S₁ und F₁, inkl. aller Interaktionen

#### 2. Die N₁-Matrix (Zustands-Policy-Link)

$$\mathbf{N}_1 = \begin{pmatrix} N_{1,Sy} & N_{1,Sd} & N_{1,Sb} & N_{1,S\theta} \\ N_{1,Fy} & N_{1,Fd} & N_{1,Fb} & N_{1,F\theta} \end{pmatrix}$$

mit Elementen wie:

$$N_{1,Sd} = -(P_{2,yd} \alpha_S - P_{2,dd} \hat{\delta}_S - P_{2,d\theta} \phi_S) \rho_d$$

**Kodiert:** Wie Zustände die optimalen Policies beeinflussen

#### 3. Die A₁-Matrix (Policy-Koeffizienten)

$$\mathbf{A}_1 = \mathbf{Q}_1^{-1} \mathbf{N}_1 = \begin{pmatrix} a_1^{S,y} & a_1^{S,d} & a_1^{S,b} & a_1^{S,\theta} \\ a_1^{F,y} & a_1^{F,d} & a_1^{F,b} & a_1^{F,\theta} \end{pmatrix}$$

**Kodiert:** Die vollständigen optimalen Reaktionsfunktionen

---

### Vergleich zu t=2

| Aspekt | t=2 | t=1 |
|--------|-----|-----|
| **Q-Matrix Quelle** | Direkt: $w_y, w_d, \tilde{w}_b$ | Indirekt: P₂-Matrix (10 Koeffizienten) |
| **Komplexität** | Niedrig (3 Parameter) | Hoch (10 P-Koeffizienten) |
| **Zeithorizont** | 1 Periode | 2 Perioden |
| **Trilemma-Schärfe** | Moderat | Intensiv |
| **Schuldenkosten** | Exogen ($\tilde{w}_b$) | Endogen ($P_{2,bb}$) |
| **Epidemie-Kosten** | Via $\delta_\theta$ nur | Via $P_{2,\theta\theta}$ + Cross-Terms |

---

### Die Rolle der P₂-Matrix

Die P₂-Matrix ist das **Herzstück** der Verbindung zwischen t=1 und t=2:

**Was P₂ kodiert:**
1. Direkte Kosten in t=2
2. Optimale Policies $S_2^*, F_2^*$
3. Transitionseffekte zu t=3
4. Terminal Kosten
5. Alle Interaktionen

**Warum das wichtig ist:**
- Ohne P₂: Müssten alle Zukunfts-Pfade einzeln berechnen
- Mit P₂: Kompakte Darstellung der gesamten Zukunft
- **Elegante Dimensionsreduktion:** 10 Koeffizienten statt unendlich vieler Pfade

---

### Ökonomische Interpretation

**Forward-Looking Behavior:**

Die Policy-Koeffizienten in t=1 sind nicht nur Reaktionen auf aktuelle Zustände, sondern reflektieren:

1. Wie die Zustände sich entwickeln (Transitionsgleichungen)
2. Wie zukünftige Policies darauf reagieren (optimale $S_2^*, F_2^*$)
3. Wie diese Reaktionen die finalen Kosten beeinflussen (Terminal costs)

**Das COVID-19 Trilemma:**

Man kann nicht gleichzeitig haben:
1. Niedrigen Gesundheitsschaden (niedriges $d$)
2. Niedrigen Output-Schaden (niedriges $y$)
3. Niedrige Schulden (niedriges $b$)

**Die Schärfe dieses Trade-offs ist in t=1 größer als in t=2, weil:**
- Mehr Perioden voraus zu berücksichtigen
- Kumulative Effekte von Schulden
- Rekursive Struktur der Policies

---

### Mathematische Eleganz

Die Lösung zeigt die Kraft der **Dynamic Programming:**

1. **Backward Induction:** Löse von hinten nach vorne
2. **Bellman Principle:** Optimale Policies für t=1 nutzen V₂*
3. **Quadratic Forms:** Kompakte Darstellung komplexer Dynamiken
4. **Linear Policies:** Certainty Equivalence macht Lösung tractable
5. **Matrix Algebra:** Elegante Notation für komplexe Strukturen

---

### Praktische Bedeutung

**Für Policymaker:**

1. **Frühe Intervention wichtig:** Je früher, desto komplexer die Trade-offs
2. **Schulden-Akkumulation kritisch:** Kumulative Effekte über Perioden
3. **Epidemie-Kontrolle lohnt sich:** Mehr Zukunft zu schützen
4. **Policy-Koordination essentiell:** S und F müssen zusammen optimiert werden

**Für Forscher:**

1. **Tractable Framework:** Lösbares Modell trotz Komplexität
2. **Quantifizierbar:** Alle Koeffizienten berechenbar
3. **Testbar:** Policy-Koeffizienten empirisch schätzbar
4. **Erweiterbar:** Framework anwendbar auf andere Krisen

---

## Anhang: Vollständige Notation

### Zustände
- $y_t$: Output-Schaden (GDP loss)
- $d_t$: Gesundheitsschaden (Todesfälle)
- $b_t$: Staatsschulden (% of GDP)
- $\theta_t$: Infektionsrate

### Instrumente
- $S_t$: Lockdown-Stringenz (0-100)
- $F_t$: Fiscal Support (% of GDP)

### Strukturparameter
- $\alpha_S, \alpha_F$: Effekte von S, F auf Output
- $\delta_S, \delta_\theta$: Lockdown-Effekt auf Gesundheit
- $\phi_S$: Lockdown-Effekt auf Infektionsrate
- $\hat{\delta}_S = \delta_S + 2\delta_\theta \phi_S$: Effektiver Lockdown-Effekt
- $\rho_y, \rho_d, \rho_\theta$: Persistenz-Parameter
- $\kappa_F, \kappa_y$: Schulden-Akkumulation
- $r$: Zinssatz

### Präferenzen
- $w_y, w_d, w_b$: Gewichte für Schäden
- $\beta$: Diskontfaktor
- $\tilde{w}_b = w_b + \frac{1}{2}k_{b3}$: Terminal Penalty

### Matrizen
- $\mathbf{P}_2$: 4×4 Value Function Matrix (aus t=2)
- $\mathbf{Q}_1$: 2×2 Instrumentenkosten-Matrix
- $\mathbf{N}_1$: 2×4 Zustands-Policy-Matrix
- $\mathbf{A}_1$: 2×4 Policy-Koeffizienten-Matrix

### Stochastik
- $\varepsilon_t \sim N(0, \sigma_t^2)$: Infektions-Schock
- $E_t[\cdot]$: Erwartungswert bedingt auf Info in t

---

*Erstellt: 2025*
*Modell: COVID-19 Trilemma - Dynamic Stochastic Optimization*
*Methode: Backward Induction mit Quadratic Approximation*
