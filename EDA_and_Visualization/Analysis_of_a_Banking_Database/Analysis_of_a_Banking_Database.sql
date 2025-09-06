/******************************************
*      Analysis of a Banking Database     *
******************************************/

-- Description --
/*
This project creates a denormalized table containing behavioral indicators for clients,
calculated based on their banking transactions and the banking products they own.
The goal is to generate features suitable for a potential supervised machine learning model.
*/

-- Indicators --
/*
The following indicators have been created for each client ID:
1  - Age
2A - Number of outgoing transactions across all accounts
2B - Number of incoming transactions across all accounts
3A - Total amount of outgoing transactions across all accounts
3B - Total amount of incoming transactions across all accounts
4  - Total number of accounts owned
5  - Number of accounts owned by type (one indicator for each type)
6A - Number of outgoing transactions by type (one indicator for each type)
6B - Number of incoming transactions by type (one indicator for each type)
7A - Total outgoing amount by account type (one indicator for each type)
7B - Total incoming amount by account type (one indicator for each type)
*/

/* MySQL version used */
select version(); -- 8.0.37


/*--------------------------------------------------------------------------------------------------------------------------------------------------*/

/*----------------*/
/* 1 - Client Age */
/*----------------*/

create temporary table banca.t1_client_age (

    -- Display client ID, birth date, and current age
    select
        id_cliente,
        data_nascita,
        timestampdiff(year, data_nascita, current_date()) as client_age -- timestamp difference in days converted in years
    from banca.cliente
);

/*----------------------------------------------------------*/
/* 2A - Number of Outgoing Transactions Across All Accounts */
/*----------------------------------------------------------*/

create temporary table banca.t2a_number_outgoing_transactions (

    -- Display client ID and count of outgoing transactions (by sign occurrences)
    select
        clients.id_cliente,
        count(transaction_types.segno) as number_outgoing_transactions
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Join transactions table by account ID
    inner join banca.transazioni transactions
    on accounts.id_conto = transactions.id_conto

    -- Join transaction types table by transaction type ID
    inner join banca.tipo_transazione transaction_types
    on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

    -- Select only transactions with negative sign
    where segno = '-'

    -- Group results by client ID
    group by clients.id_cliente
);

/*----------------------------------------------------------*/
/* 2B - Number of Incoming Transactions Across All Accounts */
/*----------------------------------------------------------*/

create temporary table banca.t2b_number_incoming_transactions (

    -- Display client ID and count of incoming transactions (by sign occurrences)
    select
        clients.id_cliente,
        count(transaction_types.segno) as number_incoming_transactions
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Join transactions table by account ID
    inner join banca.transazioni transactions
    on accounts.id_conto = transactions.id_conto

    -- Join transaction types table by transaction type ID
    inner join banca.tipo_transazione transaction_types
    on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

    -- Include only transactions with positive sign
    where segno = '+'

    -- Group results by client ID
    group by clients.id_cliente
);

/*------------------------------------------------*/
/* 3A - Total Outgoing Amount Across All Accounts */
/*------------------------------------------------*/

create temporary table banca.t3a_total_outgoing_amount (

    -- Display client ID and total outgoing amount
    select
        clients.id_cliente,
        sum(importo) as total_outgoing_amount
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Join transactions table by account ID
    inner join banca.transazioni transactions
    on accounts.id_conto = transactions.id_conto

    -- Join transaction types table by transaction type ID
    inner join banca.tipo_transazione transaction_types
    on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

    -- Select only negative amounts (outgoing transactions)
    where importo < 0

    -- Group results by client ID
    group by clients.id_cliente
);

/*------------------------------------------------*/
/* 3B - Total Incoming Amount Across All Accounts */
/*------------------------------------------------*/

create temporary table banca.t3b_total_incoming_amount (

    -- Display client ID and total incoming amount
    select
        clients.id_cliente,
        sum(importo) as total_incoming_amount
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Join transactions table by account ID
    inner join banca.transazioni transactions
    on accounts.id_conto = transactions.id_conto

    -- Join transaction types table by transaction type ID
    inner join banca.tipo_transazione transaction_types
    on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

    -- Select only positive amounts (incoming transactions)
    where importo > 0

    -- Group results by client ID
    group by clients.id_cliente
);

/*---------------------------------------------------*/
/* 4 - Total Number of Accounts Owned by Each Client */
/*---------------------------------------------------*/

create temporary table banca.t4_number_accounts (

    -- Display client ID and total number of accounts
    select
        clients.id_cliente,
        count(accounts.id_conto) as number_accounts
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Group results by client ID
    group by clients.id_cliente
);

/*------------------------------------------------------*/
/* 5 - Number of Accounts Owned by Type for Each Client */
/*------------------------------------------------------*/

create temporary table banca.t5_number_accounts_by_type (

    -- Display client ID and number of accounts by type
    select
        clients.id_cliente,
        sum(case when id_tipo_conto = 0 then 1 else 0 end) as number_Base,
        sum(case when id_tipo_conto = 1 then 1 else 0 end) as number_Business,
        sum(case when id_tipo_conto = 2 then 1 else 0 end) as number_Privati,
        sum(case when id_tipo_conto = 3 then 1 else 0 end) as number_Famiglie
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Group results by client ID
    group by clients.id_cliente
);

/*--------------------------------------------------------------*/
/* 6A - Number of Outgoing Transactions by Type for Each Client */
/*--------------------------------------------------------------*/

create temporary table banca.t6a_number_of_outgoing_transactions_by_type (

    -- Display client ID and number of outgoing transactions for each type
    select    
        clients.id_cliente,
        sum(case when id_tipo_trans = 3 then 1 else 0 end) as number_Amazon,
        sum(case when id_tipo_trans = 4 then 1 else 0 end) as number_Mutuo,
        sum(case when id_tipo_trans = 5 then 1 else 0 end) as number_Hotel,
        sum(case when id_tipo_trans = 6 then 1 else 0 end) as number_Aereo,
        sum(case when id_tipo_trans = 7 then 1 else 0 end) as number_Supermercato
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Join transactions table by account ID
    inner join banca.transazioni transactions
    on accounts.id_conto = transactions.id_conto

    -- Join transaction types table by transaction type ID
    inner join banca.tipo_transazione transaction_types
    on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

    -- Select only outgoing transactions
    where segno = '-'

    -- Group results by client ID
    group by clients.id_cliente
);

/*--------------------------------------------------------------*/
/* 6B - Number of Incoming Transactions by Type for Each Client */
/*--------------------------------------------------------------*/

create temporary table banca.t6b_number_of_incoming_transactions_by_type (

    -- Display client ID and number of incoming transactions for each type
    select
        clients.id_cliente,
        sum(case when id_tipo_trans = 0 then 1 else 0 end) as number_Stipendio,
        sum(case when id_tipo_trans = 1 then 1 else 0 end) as number_Pensione,
        sum(case when id_tipo_trans = 2 then 1 else 0 end) as number_Dividendi
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Join transactions table by account ID
    inner join banca.transazioni transactions
    on accounts.id_conto = transactions.id_conto

    -- Join transaction types table by transaction type ID
    inner join banca.tipo_transazione transaction_types
    on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

    -- Select only incoming transactions
    where segno = '+'

    -- Group results by client ID
    group by clients.id_cliente
);

/*------------------------------------------------------------*/
/* 7A - Total Outgoing Amount by Account Type for Each Client */
/*------------------------------------------------------------*/

create temporary table banca.t7a_amount_outgoing_by_account_type (

    -- Display client ID and outgoing amount by account type
    select
        clients.id_cliente,
        sum(case when id_tipo_conto = 0 then importo else 0 end) as amount_outgoing_Base,
        sum(case when id_tipo_conto = 1 then importo else 0 end) as amount_outgoing_Business,
        sum(case when id_tipo_conto = 2 then importo else 0 end) as amount_outgoing_Privati,
        sum(case when id_tipo_conto = 3 then importo else 0 end) as amount_outgoing_Famiglie
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Join transactions table by account ID
    inner join banca.transazioni transactions
    on accounts.id_conto = transactions.id_conto

    -- Join transaction types table by transaction type ID
    inner join banca.tipo_transazione transaction_types
    on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

    -- Select only outgoing transactions
    where segno = '-'

    -- Group results by client ID
    group by clients.id_cliente
);

/*------------------------------------------------------------*/
/* 7B - Total Incoming Amount by Account Type for Each Client */
/*------------------------------------------------------------*/

create temporary table banca.t7b_amount_incoming_by_account_type (

    -- Display client ID and incoming amount by account type
    select
        clients.id_cliente,
        sum(case when id_tipo_conto = 0 then importo else 0 end) as amount_incoming_Base,
        sum(case when id_tipo_conto = 1 then importo else 0 end) as amount_incoming_Business,
        sum(case when id_tipo_conto = 2 then importo else 0 end) as amount_incoming_Privati,
        sum(case when id_tipo_conto = 3 then importo else 0 end) as amount_incoming_Famiglie
    from banca.cliente clients

    -- Join accounts table by client ID
    inner join banca.conto accounts
    on clients.id_cliente = accounts.id_cliente

    -- Join transactions table by account ID
    inner join banca.transazioni transactions
    on accounts.id_conto = transactions.id_conto

    -- Join transaction types table by transaction type ID
    inner join banca.tipo_transazione transaction_types
    on transactions.id_tipo_trans = transaction_types.id_tipo_transazione

    -- Select only incoming transactions
    where segno = '+'

    -- Group results by client ID
    group by clients.id_cliente
);

/*------------------------*/
/* Final Aggregated Query */
/*------------------------*/

select
    t1.id_cliente,
    t1.client_age,
    t2a.number_outgoing_transactions,
    t2b.number_incoming_transactions,
    t3a.total_outgoing_amount,
    t3b.total_incoming_amount,
    t4.number_accounts,
    t5.number_Base,
    t5.number_Business,
    t5.number_Privati,
    t5.number_Famiglie,
    t6a.number_Amazon,
    t6a.number_Mutuo,
    t6a.number_Hotel,
    t6a.number_Aereo,
    t6a.number_Supermercato,
    t6b.number_Stipendio,
    t6b.number_Pensione,
    t6b.number_Dividendi,
    t7a.amount_outgoing_Base,
    t7a.amount_outgoing_Business,
    t7a.amount_outgoing_Privati,
    t7a.amount_outgoing_Famiglie,
    t7b.amount_incoming_Base,
    t7b.amount_incoming_Business,
    t7b.amount_incoming_Privati,
    t7b.amount_incoming_Famiglie
from banca.t1_client_age t1
left join banca.t2a_number_outgoing_transactions            t2a on t1.id_cliente = t2a.id_cliente
left join banca.t2b_number_incoming_transactions            t2b on t1.id_cliente = t2b.id_cliente
left join banca.t3a_total_outgoing_amount                   t3a on t1.id_cliente = t3a.id_cliente
left join banca.t3b_total_incoming_amount                   t3b on t1.id_cliente = t3b.id_cliente
left join banca.t4_number_accounts                          t4  on t1.id_cliente = t4.id_cliente
left join banca.t5_number_accounts_by_type                  t5  on t1.id_cliente = t5.id_cliente
left join banca.t6a_number_of_outgoing_transactions_by_type t6a on t1.id_cliente = t6a.id_cliente
left join banca.t6b_number_of_incoming_transactions_by_type t6b on t1.id_cliente = t6b.id_cliente
left join banca.t7a_amount_outgoing_by_account_type         t7a on t1.id_cliente = t7a.id_cliente
left join banca.t7b_amount_incoming_by_account_type         t7b on t1.id_cliente = t7b.id_cliente;

/*-----------------------------*/
/* Delete all temporary tables */
/*-----------------------------*/
drop temporary table if exists banca.t1_client_age;
drop temporary table if exists banca.t2a_number_outgoing_transactions;
drop temporary table if exists banca.t2b_number_incoming_transactions;
drop temporary table if exists banca.t3a_total_outgoing_amount;
drop temporary table if exists banca.t3b_total_incoming_amount;
drop temporary table if exists banca.t4_number_accounts;
drop temporary table if exists banca.t5_number_accounts_by_type;
drop temporary table if exists banca.t6a_number_of_outgoing_transactions_by_type;
drop temporary table if exists banca.t6b_number_of_incoming_transactions_by_type;
drop temporary table if exists banca.t7a_amount_outgoing_by_account_type;
drop temporary table if exists banca.t7b_amount_incoming_by_account_type;
