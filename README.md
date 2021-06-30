# cobol
Projeto 1
      *----------------------------------------------------------------*
      *OBJETIVO: ESTE PROGRAMA LE UMA SYSIN CONTEDO DADOS DE           *
      *          EMPRESTIMOS DE CLIENTES E FAZ UMA SIMULACAO           *
      *          CALCULANDO O VALOR DAS PARCELAS E DO TOTAL DO         *
      *          EMPRESTIMO SIMULLADO                                  *
      *----------------------------------------------------------------*
       IDENTIFICATION                      DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                         GPAZ9901.
       AUTHOR.                             JESSICA.
       DATE-WRITTEN.                       21/05/2021.
       DATE-COMPILED.                      24/05/2021.
       SECURITY.                           NENHUM.
      *----------------------------------------------------------------*
       ENVIRONMENT                         DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                       SECTION.

       SOURCE-COMPUTER.                    NOTEJESSICA.

       OBJECT-COMPUTER.                    NOTEJESSICA.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT                       SECTION.

      *ESTA SECAO FICARA VAZIA  POIS ESTE ARQUIVO NAO MANIPULA ARQUIVOS

      *----------------------------------------------------------------*
       DATA                                DIVISION.
      *----------------------------------------------------------------*

       FILE                                SECTION.
      *ESTA SECAO FICARA VAZIA POIS ESTE ARQUIVO NAO MANIPULA ARQUIVOS


       WORKING-STORAGE                     SECTION.
       77  WS-VAL-PARCELA                  PIC 9(08)V99.
       77  WS-VAL-TOTAL-EMP                PIC 9(10)V99.
       77  WS-CTLIDO                       PIC 9(02).
       77  WS-CTPROC                       PIC 9(02).
       01  WS-REG-SYSIN.
           05 WS-NUM-SIMULACAO             PIC 9(04).
           05 WS-VAL-EMPRESTIMO            PIC 9(06)V99.
           05 WS-JUROS                     PIC 9(02)V99.
           05 WS-QTD-PARCELAS              PIC 9(02).

       LINKAGE                             SECTION.

      *ESTA SECAO FICARA VAZIA POIS ESTE PROJETO NAO RECEBE VARIAVEIS

      *----------------------------------------------------------------*
       PROCEDURE                           DIVISION.
      *----------------------------------------------------------------*
       0000-GPAZ9901.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PROCESSAR UNTIL  WS-REG-SYSIN = ALL "0"
           PERFORM 3000-TERMINO
           STOP RUN
           .

       1000-INICIALIZAR.
           MOVE 0 TO WS-CTLIDO WS-CTPROC
           PERFORM 1500-LER-SYSIN
           .
       1500-LER-SYSIN.
           ACCEPT WS-REG-SYSIN FROM SYSIN
           IF WS-REG-SYSIN NOT = ALL  "0"
              COMPUTE WS-CTLIDO = WS-CTLIDO + 1
           END-IF
           .

       2000-PROCESSAR.
           COMPUTE WS-VAL-PARCELA =
           (WS-VAL-EMPRESTIMO * WS-JUROS / 100 )
           / (1 - 1 / ( 1 + WS-JUROS
           / 100 ) ** WS-QTD-PARCELAS)
           COMPUTE WS-VAL-TOTAL-EMP = WS-VAL-PARCELA *
           WS-QTD-PARCELAS
           DISPLAY "***************************************************"
           DISPLAY "NUMERO DA SIMULACAO    : "  WS-NUM-SIMULACAO
           DISPLAY " VALOR DO EMPRESTIMO   : "  WS-VAL-EMPRESTIMO

           DISPLAY " JUROS                 : "  WS-JUROS
           DISPLAY " QTDE DE PARCELAS      : "  WS-QTD-PARCELAS
           DISPLAY " VALOR DA PARCELA      : "  WS-VAL-PARCELA
           DISPLAY " VALOR TOTAL           : "  WS-VAL-TOTAL-EMP
           DISPLAY "***************************************************"
           COMPUTE  WS-CTPROC = WS-CTPROC + 1
           PERFORM 1500-LER-SYSIN
           .

       3000-TERMINO.
           DISPLAY "***************************************************"
           DISPLAY " TOTAL DE SIMULACOES LIDAS   : " WS-CTLIDO
           DISPLAY " TOTAL DE SIMULACOES PROCESSADAS :" WS-CTPROC
           DISPLAY " **************************************************"
           DISPLAY " TERMINO NORMAL DE PROCESSAMENTO DO TEST "
           DISPLAY "***************************************************"
           .
