       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXE06.
       DATE-WRITTEN.  20/03/2024.
       DATE-COMPILED.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CAD-ENT   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
           
           SELECT CAD-SAI   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD CAD-ENT
           LABEL  RECORD  ARE STANDARD
           VALUE  OF FILE-ID IS "CAD-ENT.DAT".
       
       01 REG-ENT.
           03 MATRICULA-ENT     PIC 9(05).
           03 NOME-ENT          PIC X(30).
           03 SALARIO-ENT       PIC 9(05)V99.
       
       FD CAD-SAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-SAI.DAT".

       01 REG-SAI.
           03 MATRICULA-SAI     PIC 9(05).
           03 NOME-SAI          PIC X(30).
           03 SALARIO-SAI       PIC 9(05)V99.

       WORKING-STORAGE SECTION.
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       
       PROCEDURE DIVISION.
       PGM-EM01EX06.
           PERFORM INICIO.
           PERFORM PRINCIPAL
                  UNTIL FIM-ARQ EQUAL "SIM".

           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CAD-ENT
                OUTPUT CAD-SAI.
           PERFORM LEITURA.

       LEITURA.
           READ CAD-ENT
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           IF (SALARIO-ENT > 3000)
               MOVE MATRICULA-ENT TO MATRICULA-SAI
               MOVE NOME-ENT TO NOME-SAI
               MOVE SALARIO-ENT TO SALARIO-SAI
               WRITE REG-SAI.

       TERMINO.
           CLOSE  CAD-ENT
                  CAD-SAI.
