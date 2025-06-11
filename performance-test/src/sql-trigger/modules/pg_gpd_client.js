const { ENTITY_IDENTIFIER } = require('./common.js');
const { Pool } = require('pg');

//COMMON
const serverName = process.env.PG_GPD_SERVER_NAME;
const databaseName = process.env.PG_GPD_DATABASE_NAME;
//SECRETS
const password = process.env.PG_GPD_PASSWORD;
const username = process.env.PG_GPD_USERNAME;

const pool = new Pool({
  user: username,
  database: databaseName,
  password: password,
  host: serverName,
  port: 5432,
  ssl: false
});

const connection = {
  pool,
  query: (...args) => {
    return pool.connect().then((client) => {
      return client.query(...args).then((res) => {
        client.release();
        return res.rows;
      });
    });
  },
};

async function shutDownPool() {
  await pool.end();
}

async function insertPaymentPositionWithValidFiscalCode(id) {
  await connection.query(`INSERT INTO apd.payment_position (id, city, civic_number, company_name, country, email, fiscal_code, full_name, inserted_date, iupd, last_updated_date, max_due_date, min_due_date, office_name, organization_fiscal_code, phone, postal_code, province, publish_date, region, status, street_name, "type", validity_date, "version", switch_to_expired, payment_date, pull, pay_stand_in, service_type) VALUES('${id}', 'Roma', '1', 'SkyLab Inc. - Edit', 'IT', 'marina.verdi@mail.com', 'VRDMRN92A12H501Z', 'Marina Verdi', '2025-06-10 16:09:43.477', '${ENTITY_IDENTIFIER}', '2025-06-10 16:09:43.479', '2025-06-10 16:09:43.323', '2025-06-10 16:09:43.323', 'SkyLab - Sede via Washington - Edit', 'ORG_FISCAL_CODE_${id}', '333-123456789', '00100', 'RM', '2025-06-10 16:09:43.479', NULL, 'VALID', 'Via della Conciliazione', 'F', '2025-06-10 16:09:43.479', 0, false, NULL, true, false, 'GPD');`);
}
async function updatePaymentPositionStatus() {
  await connection.query(`UPDATE apd.payment_position SET status='PAID' WHERE iupd='${ENTITY_IDENTIFIER}'`);
}
async function insertPaymentOption(id) {
  await connection.query(`INSERT INTO apd.payment_option (id, amount, description, due_date, fee, flow_reporting_id, receipt_id, inserted_date, is_partial_payment, iuv, last_updated_date, organization_fiscal_code, payment_date, payment_method, psp_company, reporting_date, retention_date, status, payment_position_id, notification_fee, last_updated_date_notification_fee, nav, fiscal_code, full_name, "type", street_name, civic_number, postal_code, city, province, region, country, email, phone, send_sync, psp_code, psp_tax_code, payment_position_status) VALUES('${id}', 123, '${ENTITY_IDENTIFIER}', '2025-06-10 08:07:43.000', 0, NULL, NULL, '2025-06-10 08:07:44.052', false, '512472450730077', '2025-06-10 12:22:55.072', 'ORG_FISCAL_CODE_${id}', NULL, NULL, NULL, NULL, NULL, 'PO_UNPAID', '${id}', 0, NULL, '348174669166373035', 'VRDMRN92A12H501Z', 'Marina Verdi', 'F', 'Via della Conciliazione', '1', '00100', 'Roma', 'RM', NULL, 'IT', 'marina.verdi@mail.com', '333-123456789', false, NULL, NULL, 'DRAFT');`);
}

async function deletePaymentPositions() {
  await connection.query(`DELETE FROM apd.payment_position WHERE iupd='${ENTITY_IDENTIFIER}'`);
}
async function deletePaymentOptions() {
  await connection.query(`DELETE FROM apd.payment_option WHERE description='${ENTITY_IDENTIFIER}'`);
}

module.exports = {
  shutDownPool,
  insertPaymentPositionWithValidFiscalCode, updatePaymentPositionStatus, insertPaymentOption, deletePaymentPositions,deletePaymentOptions
}