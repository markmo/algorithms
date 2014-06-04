#Pkg.add("Debug")

#using Debug

function countinversions(a, n=length(a))
    if n == 1
        (a, 0)
    else
        l = (int)(n / 2)
        m = n - l
        (sx, x) = countinversions(a[1:l], l)
        (sy, y) = countinversions(a[(l + 1):n], m)
        (sz, z) = countsplitinversions(sx, l, sy, m, n)
        (sz, x + y + z)
    end
end

function countsplitinversions(b, blen, c, clen, n)
    s = zeros(Int, n)
    x = 0
    i, j = 1, 1
    for k in 1:n
        if i > blen
            s[k] = c[j]
            j += 1
        elseif j > clen
            s[k] = b[i]
            i += 1
        elseif b[i] < c[j]
            s[k] = b[i]
            i += 1
        else
            s[k] = c[j]
            j += 1
            x += blen - i + 1
        end
    end
    (s, x)
end
