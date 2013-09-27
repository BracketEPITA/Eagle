class neuron =
    object (this)
        val mutable inputs = ([] : (int * neuron) list)
        val mutable outputs = ([] : (int * neuron) list)
        method sigma x = 1. /. (1. +. exp(x))
    end;;
