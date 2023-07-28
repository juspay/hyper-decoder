export function lookupVal(fn) {
    return (key) => {
        return fn[key]
    }
}

export const getType = (obj) => typeof obj

export function isInt(n){ return (n | 0) === n }

export const isNullOrUndefined = (obj) => obj === undefined || obj === null

export const isArray = (obj) => Array.isArray(obj)

export function arrDecodeImpl(arr) {
    return (decodeFn) => {
        return (success) => {
            return (failure) => {
                // exception handling needed as we are deliberately passing a error throwing failure function in decodeFn
                try {
                    const arrToRet = [];
                    arr.forEach(curr => arrToRet.push(decodeFn(curr)))
                    return success(arrToRet);
                } catch (e) {
                    return failure(e.message)
                }
            }
        }
    }
}

export function tryCatch(obj) {
    return (recordDecodeFn) => {
        return (success) => {
            return (failure) => {
                try {
                    return success(recordDecodeFn(obj))
                } catch (e) {
                    return failure(e.message)
                }
            }
        }
    }
}

export function shortCircuit(err) {
    throw new Error(err)
}

export function tryWithString(str) {
    return (decodeFn) => {
        return (failure) => {
            try {
                return decodeFn(JSON.parse(str))
            } catch (e) {
                return failure(e.message)
            }
        }
    }
}

export const mkDecodeEntry = function (k) {
    return (fn) => {
        return (gn) => {
            return [k, fn, gn]
        }
    }
}

function constructData(rows, obj, success, failure) {
    try {
        const val = {}
        rows.forEach((x) => {
            val[x[0]] = x[1](obj[x[0]])((y) => y)((e) => {
                throw new Error(`while decoding ${x[0]} \n ${e}`)
            })
        })
        return success(val)
    } catch (e) {
        return failure(e.message)
    }
}

export function constructFromIterativeForm(rows) {
    return (obj) => {
        return (success) => {
            return (failure) => {
                return constructData(rows.decoders, obj, success, failure);
            }
        }
    }
}

export function arrayPush(arr) {
    return (a) => {
        try {
            arr.push(a)
            return arr
        } catch (e) {
            console.error("error while inserting into array " + e.message)
            return arr;
        }
    }
}

export function createIterativeForm(arr) {
    const partialDecoders = {}
    arr.map((x) => {
        partialDecoders[x[0]] = x[2]
    })
    return {decoders: arr, partialDecoders: partialDecoders};
}


function constructPartial(typeData, __base, obj, success, failure) {
    try {
        if(Object.keys(typeData).length === 0) return success({}); // todo : check if this is performant
        if (typeof obj !== "object") return failure(`type mismatch : expected object, found ${typeof obj}`);
        if (Array.isArray(obj)) return failure("type mismatch : expected object, found array");
        if (obj === null) return failure("type mismatch : expected object, found null");
        const base = {}
        for (const key in __base) {
            base[key] = __base[key]
        }
        // todo : check if this is the fastest way
        for (var key in obj) {
            const partialDecodeFn = typeData[key];
            const baseVal = base[key];
            if (partialDecodeFn === undefined && baseVal === undefined) {
                // console.log(`for key ${key}, unable to find partial decode function`)
                continue;
            }
            base[key] = partialDecodeFn(baseVal)(obj[key])((successVal) => successVal)((e) => {
                throw new Error(`while decoding ${key} \n ${e}`)
            })
        }
        return success(base);
    } catch (e) {
        return failure(e.message)
    }
}

export function constructFromRecData(typeData) {
    return (base) => {
        return (obj) => {
            return (success) => {
                return (failure) => {
                    return constructPartial(typeData.partialDecoders, base, obj, success, failure);
                }
            }
        }
    }
}

// hyperDecode {}
// partialDecode {}
