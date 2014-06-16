

\begin{code}
patternMatchExample :: IO ()
patternMatchExample = do
        let list = [1,2,3]
        let first:rest = list
        print first
        print rest
\end{code}
