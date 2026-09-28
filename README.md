# Modelagem-relacional-e-an-lise-de-redes-de-coautoria-em-PPGs-Projeto-PIBIC-IME-
Modelagem relacional e análise de redes de coautoria em PPGs (Projeto PIBIC — IME) com ênfase em engenharia ambiental

# Modelagem Relacional para Análise de Redes de Coautoria e Integração Científica em PPGs

Proposta e implementação de um modelo de dados relacional voltado para o armazenamento de produções acadêmicas, orientações e colaborações em Programas de Pós-Graduação (PPGs), servindo de base para a geração dinâmica de redes de coautoria via linguagem R.

## 📌 Contexto do Projeto
* **Instituição:** Instituto de Matemática e Estatística (IME)
* **Modalidade:** Projeto PIBIC
* **Orientador:** Prof. Luís Bauman
* **Desenvolvedor:** Igor

---

## 🗄️ Estrutura do Banco de Dados

O banco de dados é composto por 4 tabelas principais:
* `PESQUISADORES`: Registro de docentes, discentes e atributos institucionais.
* `ATIVIDADES`: Registro de artigos, capítulos, projetos e publicações.
* `PARTICIPACAO`: Tabela associativa que define autoria, papéis e ordem em produções.
* `VINCULOS`: Registro de relações formais acadêmicas (ex: orientações, bancas).

---

## 🛠️ Tecnologias Utilizadas
* **SQL:** Modelagem física do banco de dados (DDL/DML).
* **LaTeX / TikZ:** Documentação técnica e geração do Diagrama Entidade-Relacionamento.
* **R (Em integração):** Construção da matriz de adjacência $W_{ij}$ e cálculo de métricas de centralidade.

---

## 🚀 Como Executar os Scripts SQL

1. Clone o repositório:
   ```bash
   git clone [https://github.com/Igor-ht-1/pibiti.git](https://github.com/Igor-ht-1/pibiti.git)

Execute o script de criação do esquema:
    \i sql/schema.sql
Povoe a base com os dados de exemplo:
    \i sql/seed.sql

##📄 Documentação Técnica

A documentação completa do projeto, incluindo o Dicionário de Dados e as Regras de Negócio, está disponível em formato PDF na pasta docs/.
