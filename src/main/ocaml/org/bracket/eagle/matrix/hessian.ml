type hessian = {
    network : Network.network;
    mutable matrix : float Matrix.matrix;
}

let update values hessian =
    let weightCount = Array.length hessian.network.weights in
    for i = 0 to weightCount do 
        for j = 0 to weightCount do
            hessian.matrix.(i).(j) +=
