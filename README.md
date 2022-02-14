# SymmetricPriorMPM

In the Multinomial Probit Model framework, the choice of a base category is necessary
to uniquely identify the parameters of the model. On the other hand, it has been shown
that this choice can strongly affect the performance of the model itself. To solve this
problem, a symmetric prior has been proposed, which introduces a new identification
strategy and allows to build a Gibbs sampler. This algorithm permits to sample in an
efficient way all the quantities involved in the model and obtain better performance with
respect to the classical asymmetric approach.