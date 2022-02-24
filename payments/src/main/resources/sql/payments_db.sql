--
-- Name: debitor; Type: TABLE; Schema: public; Owner: pagopauser
--
CREATE TABLE public.debitor (
    id bigint NOT NULL,
    address character varying(255) NOT NULL,
    area character varying(255) NOT NULL,
    cap character varying(255) NOT NULL,
    country character varying(255) NOT NULL,
    email character varying(255) NOT NULL,
    fiscal_code character varying(255) NOT NULL,
    id_tenant character varying(255),
    name character varying(255) NOT NULL,
    number character varying(255) NOT NULL,
    phone character varying(255) NOT NULL,
    province character varying(255) NOT NULL,
    surname character varying(255) NOT NULL,
    type integer NOT NULL
);

--
-- Name: debitor_id_seq; Type: SEQUENCE; Schema: public; Owner: pagopauser
--
CREATE SEQUENCE public.debitor_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: debitor_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: pagopauser
--
ALTER SEQUENCE public.debitor_id_seq OWNED BY public.debitor.id;

--
-- Name: incrementaliuvnumber; Type: TABLE; Schema: public; Owner: pagopauser
--
CREATE TABLE public.incrementaliuvnumber (
    id bigint NOT NULL,
    anno integer NOT NULL,
    iddominiopa character varying(255) NOT NULL,
    lastusednumber bigint NOT NULL
);

ALTER TABLE public.incrementaliuvnumber OWNER TO pagopauser;

--
-- Name: incrementaliuvnumber_id_seq; Type: SEQUENCE; Schema: public; Owner: pagopauser
--
CREATE SEQUENCE public.incrementaliuvnumber_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: incrementaliuvnumber_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: pagopauser
--
ALTER SEQUENCE public.incrementaliuvnumber_id_seq OWNED BY public.incrementaliuvnumber.id;

--
-- Name: payment_options; Type: TABLE; Schema: public; Owner: pagopauser
--
CREATE TABLE public.payment_options (
    id bigint NOT NULL,
    all_cpp boolean NOT NULL,
    amount numeric(19, 2) NOT NULL,
    duo_date date NOT NULL,
    fee numeric(19, 2),
    fiscal_code character varying(255) NOT NULL,
    is_conclusive boolean NOT NULL,
    metadata character varying(255),
    notification_code character varying(255) NOT NULL,
    payment_date timestamp without time zone,
    payment_method character varying(255),
    psp_company_name character varying(255),
    receipt_id character varying(255),
    receipt text,
    retention_date date,
    status integer NOT NULL,
    payment_position_id bigint,
    id_flow_reporting character varying(255),
    date_reporting date
);

--
-- Name: payment_options_id_seq; Type: SEQUENCE; Schema: public; Owner: pagopauser
--
CREATE SEQUENCE public.payment_options_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: payment_options_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: pagopauser
--
ALTER SEQUENCE public.payment_options_id_seq OWNED BY public.payment_options.id;

--
-- Name: payment_position; Type: TABLE; Schema: public; Owner: pagopauser
--
CREATE TABLE public.payment_position (
    id bigint NOT NULL,
    amount numeric(19, 2) NOT NULL,
    company_name character varying(255),
    description character varying(255),
    information character varying(255),
    insert_date timestamp without time zone NOT NULL,
    job_id bigint NOT NULL,
    office_name character varying(255),
    organization_fiscal_code character varying(255) NOT NULL,
    paid_options integer NOT NULL,
    publish_date date,
    reported_options integer NOT NULL,
    status integer NOT NULL,
    total_options integer NOT NULL,
    debitor_id bigint
);

--
-- Name: payment_position_id_seq; Type: SEQUENCE; Schema: public; Owner: pagopauser
--
CREATE SEQUENCE public.payment_position_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: payment_position_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: pagopauser
--
ALTER SEQUENCE public.payment_position_id_seq OWNED BY public.payment_position.id;

--
-- Name: transfers; Type: TABLE; Schema: public; Owner: pagopauser
--
CREATE TABLE public.transfers (
    id bigint NOT NULL,
    iban character varying(255),
    organization_fiscal_code character varying(255) NOT NULL,
    partial_amount numeric(19, 2) NOT NULL,
    postal_auth_code character varying(255),
    postal_iban character varying(255),
    postal_iban_holder character varying(255),
    reason character varying(255) NOT NULL,
    taxonomy character varying(255) NOT NULL,
    payment_option_id bigint,
    CONSTRAINT transfers_check CHECK (((postal_iban IS NOT NULL) OR (iban IS NOT NULL)))
);

--
-- Name: transfers_id_seq; Type: SEQUENCE; Schema: public; Owner: pagopauser
--
CREATE SEQUENCE public.transfers_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

--
-- Name: transfers_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: pagopauser
--
ALTER SEQUENCE public.transfers_id_seq OWNED BY public.transfers.id;

--
-- Name: debitor id; Type: DEFAULT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.debitor
    ALTER COLUMN id SET DEFAULT nextval('public.debitor_id_seq'::regclass);

--
-- Name: incrementaliuvnumber id; Type: DEFAULT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.incrementaliuvnumber
    ALTER COLUMN id SET DEFAULT nextval('public.incrementaliuvnumber_id_seq'::regclass);

--
-- Name: payment_options id; Type: DEFAULT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.payment_options
    ALTER COLUMN id SET DEFAULT nextval('public.payment_options_id_seq'::regclass);

--
-- Name: payment_position id; Type: DEFAULT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.payment_position
    ALTER COLUMN id SET DEFAULT nextval('public.payment_position_id_seq'::regclass);

--
-- Name: transfers id; Type: DEFAULT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.transfers
    ALTER COLUMN id SET DEFAULT nextval('public.transfers_id_seq'::regclass);

--
-- Name: debitor debitor_pkey; Type: CONSTRAINT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.debitor
    ADD CONSTRAINT debitor_pkey PRIMARY KEY (id);

--
-- Name: incrementaliuvnumber incrementaliuvnumber_pkey; Type: CONSTRAINT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.incrementaliuvnumber
    ADD CONSTRAINT incrementaliuvnumber_pkey PRIMARY KEY (id);

--
-- Name: payment_options payment_options_pkey; Type: CONSTRAINT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.payment_options
    ADD CONSTRAINT payment_options_pkey PRIMARY KEY (id);

--
-- Name: payment_position payment_position_pkey; Type: CONSTRAINT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.payment_position
    ADD CONSTRAINT payment_position_pkey PRIMARY KEY (id);

--
-- Name: transfers transfers_pkey; Type: CONSTRAINT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.transfers
    ADD CONSTRAINT transfers_pkey PRIMARY KEY (id);

--
-- Name: payment_position fk7lxje2kvf0mu0s0pkd41tnmi0; Type: FK CONSTRAINT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.payment_position
    ADD CONSTRAINT fk7lxje2kvf0mu0s0pkd41tnmi0 FOREIGN KEY (debitor_id) REFERENCES public.debitor (id);

--
-- Name: payment_options fkfxwky1geuvnd6tnaejpspgymp; Type: FK CONSTRAINT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.payment_options
    ADD CONSTRAINT fkfxwky1geuvnd6tnaejpspgymp FOREIGN KEY (payment_position_id) REFERENCES public.payment_position (id);

--
-- Name: transfers fkh7ec3w5397l5w65mc3w3vnpk5; Type: FK CONSTRAINT; Schema: public; Owner: pagopauser
--
ALTER TABLE ONLY public.transfers
    ADD CONSTRAINT fkh7ec3w5397l5w65mc3w3vnpk5 FOREIGN KEY (payment_option_id) REFERENCES public.payment_options (id);
