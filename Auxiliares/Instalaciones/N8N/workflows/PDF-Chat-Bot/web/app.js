// ========================================
// PDF-Chat-Bot - JavaScript Application
// ========================================

class PDFChatBot {
    constructor() {
        this.apiUrl = 'http://localhost:5678/webhook';
        this.sessionId = this.generateSessionId();
        this.userId = 'user_' + Date.now();
        this.conversationId = null;
        this.isTyping = false;
        
        this.initializeElements();
        this.bindEvents();
        this.checkSystemStatus();
    }

    // Inicializar elementos del DOM
    initializeElements() {
        this.elements = {
            messageInput: document.getElementById('messageInput'),
            sendButton: document.getElementById('sendMessage'),
            messagesContainer: document.getElementById('messagesContainer'),
            welcomeMessage: document.getElementById('welcomeMessage'),
            typingIndicator: document.getElementById('typingIndicator'),
            clearChat: document.getElementById('clearChat'),
            showHistory: document.getElementById('showHistory'),
            statusIndicator: document.getElementById('statusIndicator'),
            charCount: document.getElementById('charCount'),
            loadingOverlay: document.getElementById('loadingOverlay'),
            toastContainer: document.getElementById('toastContainer'),
            
            // Modales
            historyModal: document.getElementById('historyModal'),
            uploadModal: document.getElementById('uploadModal'),
            closeHistory: document.getElementById('closeHistory'),
            closeUpload: document.getElementById('closeUpload'),
            
            // Upload
            attachFile: document.getElementById('attachFile'),
            uploadArea: document.getElementById('uploadArea'),
            fileInput: document.getElementById('fileInput'),
            uploadProgress: document.getElementById('uploadProgress'),
            progressFill: document.getElementById('progressFill'),
            progressText: document.getElementById('progressText'),
            
            // History
            historyList: document.getElementById('historyList'),
            historySearch: document.getElementById('historySearch')
        };
    }

    // Vincular eventos
    bindEvents() {
        // Envío de mensajes
        this.elements.sendButton.addEventListener('click', () => this.sendMessage());
        this.elements.messageInput.addEventListener('keydown', (e) => {
            if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
                e.preventDefault();
                this.sendMessage();
            }
        });

        // Auto-resize del textarea
        this.elements.messageInput.addEventListener('input', (e) => {
            this.updateCharCount();
            this.autoResizeTextarea(e.target);
        });

        // Botones de acción
        this.elements.clearChat.addEventListener('click', () => this.clearChat());
        this.elements.showHistory.addEventListener('click', () => this.showHistoryModal());
        this.elements.attachFile.addEventListener('click', () => this.showUploadModal());

        // Modales
        this.elements.closeHistory.addEventListener('click', () => this.hideModal('historyModal'));
        this.elements.closeUpload.addEventListener('click', () => this.hideModal('uploadModal'));

        // Cerrar modales al hacer clic fuera
        [this.elements.historyModal, this.elements.uploadModal].forEach(modal => {
            modal.addEventListener('click', (e) => {
                if (e.target === modal) {
                    this.hideModal(modal.id);
                }
            });
        });

        // Upload de archivos
        this.elements.uploadArea.addEventListener('click', () => this.elements.fileInput.click());
        this.elements.fileInput.addEventListener('change', (e) => this.handleFileUpload(e.files));
        
        // Drag & Drop
        this.elements.uploadArea.addEventListener('dragover', (e) => {
            e.preventDefault();
            this.elements.uploadArea.classList.add('dragover');
        });
        
        this.elements.uploadArea.addEventListener('dragleave', () => {
            this.elements.uploadArea.classList.remove('dragover');
        });
        
        this.elements.uploadArea.addEventListener('drop', (e) => {
            e.preventDefault();
            this.elements.uploadArea.classList.remove('dragover');
            this.handleFileUpload(e.dataTransfer.files);
        });

        // Búsqueda en historial
        this.elements.historySearch.addEventListener('input', (e) => {
            this.searchHistory(e.target.value);
        });

        // Preguntas de ejemplo
        document.addEventListener('click', (e) => {
            if (e.target.closest('.example-questions li')) {
                const question = e.target.textContent.replace(/[""]/g, '');
                this.elements.messageInput.value = question;
                this.elements.messageInput.focus();
                this.updateCharCount();
            }
        });
    }

    // Generar ID de sesión único
    generateSessionId() {
        return 'session_' + Date.now() + '_' + Math.random().toString(36).substr(2, 9);
    }

    // Verificar estado del sistema
    async checkSystemStatus() {
        try {
            const response = await fetch(`${this.apiUrl}/status`);
            const isOnline = response.ok;
            this.updateStatusIndicator(isOnline);
        } catch (error) {
            this.updateStatusIndicator(false);
        }
    }

    // Actualizar indicador de estado
    updateStatusIndicator(isOnline) {
        const statusDot = this.elements.statusIndicator.querySelector('.status-dot');
        const statusText = this.elements.statusIndicator.querySelector('.status-text');
        
        if (isOnline) {
            statusDot.style.backgroundColor = 'var(--success-color)';
            statusText.textContent = 'Conectado';
        } else {
            statusDot.style.backgroundColor = 'var(--error-color)';
            statusText.textContent = 'Desconectado';
        }
    }

    // Actualizar contador de caracteres
    updateCharCount() {
        const count = this.elements.messageInput.value.length;
        this.elements.charCount.textContent = `${count}/2000`;
        
        if (count > 1800) {
            this.elements.charCount.style.color = 'var(--warning-color)';
        } else if (count >= 2000) {
            this.elements.charCount.style.color = 'var(--error-color)';
        } else {
            this.elements.charCount.style.color = 'var(--text-secondary)';
        }

        // Habilitar/deshabilitar botón de envío
        this.elements.sendButton.disabled = count === 0 || count > 2000 || this.isTyping;
    }

    // Auto-resize del textarea
    autoResizeTextarea(textarea) {
        textarea.style.height = 'auto';
        textarea.style.height = Math.min(textarea.scrollHeight, 120) + 'px';
    }

    // Enviar mensaje
    async sendMessage() {
        const message = this.elements.messageInput.value.trim();
        if (!message || this.isTyping) return;

        // Ocultar mensaje de bienvenida
        this.elements.welcomeMessage.style.display = 'none';

        // Agregar mensaje del usuario
        this.addMessage(message, 'user');
        
        // Limpiar input
        this.elements.messageInput.value = '';
        this.updateCharCount();
        this.autoResizeTextarea(this.elements.messageInput);

        // Mostrar indicador de escritura
        this.showTypingIndicator();

        try {
            const response = await fetch(`${this.apiUrl}/chatbot`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({
                    question: message,
                    user_id: this.userId,
                    session_id: this.sessionId,
                    conversation_id: this.conversationId
                })
            });

            if (!response.ok) {
                throw new Error(`HTTP error! status: ${response.status}`);
            }

            const data = await response.json();
            
            // Actualizar conversation_id si es nuevo
            if (data.conversation_id) {
                this.conversationId = data.conversation_id;
            }

            // Agregar respuesta del asistente
            this.addMessage(data.response, 'assistant', {
                sources: data.sources || [],
                metadata: data.metadata || {}
            });

        } catch (error) {
            console.error('Error sending message:', error);
            this.addMessage(
                'Lo siento, ocurrió un error al procesar tu pregunta. Por favor, inténtalo de nuevo.',
                'assistant',
                { isError: true }
            );
            this.showToast('Error al enviar mensaje', 'error');
        } finally {
            this.hideTypingIndicator();
        }
    }

    // Agregar mensaje al chat
    addMessage(content, role, options = {}) {
        const messageDiv = document.createElement('div');
        messageDiv.className = `message ${role}`;

        const avatar = document.createElement('div');
        avatar.className = 'message-avatar';
        avatar.innerHTML = role === 'user' ? '<i class="fas fa-user"></i>' : '<i class="fas fa-robot"></i>';

        const messageContent = document.createElement('div');
        messageContent.className = 'message-content';

        const bubble = document.createElement('div');
        bubble.className = 'message-bubble';
        bubble.textContent = content;

        const messageInfo = document.createElement('div');
        messageInfo.className = 'message-info';
        
        const timestamp = new Date().toLocaleTimeString('es-ES', { 
            hour: '2-digit', 
            minute: '2-digit' 
        });
        
        messageInfo.innerHTML = `<span>${timestamp}</span>`;

        messageContent.appendChild(bubble);
        messageContent.appendChild(messageInfo);

        // Agregar fuentes si existen
        if (options.sources && options.sources.length > 0) {
            const sourcesDiv = document.createElement('div');
            sourcesDiv.className = 'message-sources';
            sourcesDiv.innerHTML = '<h4>Fuentes:</h4>';
            
            options.sources.forEach(source => {
                const sourceTag = document.createElement('span');
                sourceTag.className = 'source-tag';
                sourceTag.textContent = source.filename || source;
                sourcesDiv.appendChild(sourceTag);
            });
            
            messageContent.appendChild(sourcesDiv);
        }

        // Agregar metadata si existe
        if (options.metadata && options.metadata.tokens_used) {
            const metaSpan = document.createElement('span');
            metaSpan.textContent = `${options.metadata.tokens_used} tokens`;
            messageInfo.appendChild(metaSpan);
        }

        messageDiv.appendChild(avatar);
        messageDiv.appendChild(messageContent);

        this.elements.messagesContainer.appendChild(messageDiv);
        this.scrollToBottom();
    }

    // Mostrar indicador de escritura
    showTypingIndicator() {
        this.isTyping = true;
        this.elements.typingIndicator.style.display = 'flex';
        this.updateCharCount();
        this.scrollToBottom();
    }

    // Ocultar indicador de escritura
    hideTypingIndicator() {
        this.isTyping = false;
        this.elements.typingIndicator.style.display = 'none';
        this.updateCharCount();
    }

    // Scroll al final del chat
    scrollToBottom() {
        setTimeout(() => {
            this.elements.messagesContainer.scrollTop = this.elements.messagesContainer.scrollHeight;
        }, 100);
    }

    // Limpiar chat
    clearChat() {
        if (confirm('¿Estás seguro de que quieres limpiar el chat?')) {
            this.elements.messagesContainer.innerHTML = '';
            this.elements.welcomeMessage.style.display = 'flex';
            this.conversationId = null;
            this.sessionId = this.generateSessionId();
            this.showToast('Chat limpiado', 'success');
        }
    }

    // Mostrar modal
    showModal(modalId) {
        const modal = document.getElementById(modalId);
        modal.classList.add('show');
        document.body.style.overflow = 'hidden';
    }

    // Ocultar modal
    hideModal(modalId) {
        const modal = document.getElementById(modalId);
        modal.classList.remove('show');
        document.body.style.overflow = 'auto';
    }

    // Mostrar modal de historial
    async showHistoryModal() {
        this.showModal('historyModal');
        await this.loadHistory();
    }

    // Mostrar modal de upload
    showUploadModal() {
        this.showModal('uploadModal');
    }

    // Cargar historial
    async loadHistory() {
        try {
            const response = await fetch(`${this.apiUrl}/history/${this.userId}`);
            if (!response.ok) throw new Error('Error loading history');
            
            const history = await response.json();
            this.renderHistory(history);
        } catch (error) {
            console.error('Error loading history:', error);
            this.elements.historyList.innerHTML = '<p>Error al cargar el historial</p>';
        }
    }

    // Renderizar historial
    renderHistory(history) {
        if (!history || history.length === 0) {
            this.elements.historyList.innerHTML = '<p>No hay conversaciones anteriores</p>';
            return;
        }

        this.elements.historyList.innerHTML = history.map(item => `
            <div class="history-item" data-conversation-id="${item.conversation_id}">
                <div class="history-item-title">${item.title}</div>
                <div class="history-item-preview">${item.preview}</div>
                <div class="history-item-meta">
                    <span>${new Date(item.created_at).toLocaleDateString('es-ES')}</span>
                    <span>${item.message_count} mensajes</span>
                </div>
            </div>
        `).join('');

        // Agregar eventos de clic
        this.elements.historyList.querySelectorAll('.history-item').forEach(item => {
            item.addEventListener('click', () => {
                const conversationId = item.dataset.conversationId;
                this.loadConversation(conversationId);
                this.hideModal('historyModal');
            });
        });
    }

    // Buscar en historial
    searchHistory(query) {
        const items = this.elements.historyList.querySelectorAll('.history-item');
        items.forEach(item => {
            const text = item.textContent.toLowerCase();
            const matches = text.includes(query.toLowerCase());
            item.style.display = matches ? 'block' : 'none';
        });
    }

    // Cargar conversación
    async loadConversation(conversationId) {
        try {
            const response = await fetch(`${this.apiUrl}/conversation/${conversationId}`);
            if (!response.ok) throw new Error('Error loading conversation');
            
            const conversation = await response.json();
            this.renderConversation(conversation);
            this.conversationId = conversationId;
        } catch (error) {
            console.error('Error loading conversation:', error);
            this.showToast('Error al cargar la conversación', 'error');
        }
    }

    // Renderizar conversación
    renderConversation(conversation) {
        this.elements.messagesContainer.innerHTML = '';
        this.elements.welcomeMessage.style.display = 'none';

        conversation.messages.forEach(message => {
            this.addMessage(message.content, message.role, {
                sources: message.sources || [],
                metadata: message.metadata || {}
            });
        });
    }

    // Manejar upload de archivos
    async handleFileUpload(files) {
        const validFiles = Array.from(files).filter(file => {
            const validTypes = ['application/pdf', 'image/jpeg', 'image/png', 'image/jpg'];
            const maxSize = 50 * 1024 * 1024; // 50MB
            
            if (!validTypes.includes(file.type)) {
                this.showToast(`Tipo de archivo no soportado: ${file.name}`, 'error');
                return false;
            }
            
            if (file.size > maxSize) {
                this.showToast(`Archivo muy grande: ${file.name}`, 'error');
                return false;
            }
            
            return true;
        });

        if (validFiles.length === 0) return;

        this.elements.uploadProgress.style.display = 'block';
        
        for (let i = 0; i < validFiles.length; i++) {
            const file = validFiles[i];
            await this.uploadFile(file, i + 1, validFiles.length);
        }

        this.elements.uploadProgress.style.display = 'none';
        this.hideModal('uploadModal');
        this.showToast(`${validFiles.length} archivo(s) subido(s) exitosamente`, 'success');
    }

    // Subir archivo individual
    async uploadFile(file, current, total) {
        const formData = new FormData();
        formData.append('file', file);
        formData.append('user_id', this.userId);

        try {
            const response = await fetch(`${this.apiUrl}/upload`, {
                method: 'POST',
                body: formData
            });

            if (!response.ok) throw new Error('Upload failed');

            const progress = (current / total) * 100;
            this.elements.progressFill.style.width = `${progress}%`;
            this.elements.progressText.textContent = `${Math.round(progress)}%`;

        } catch (error) {
            console.error('Error uploading file:', error);
            this.showToast(`Error subiendo ${file.name}`, 'error');
        }
    }

    // Mostrar toast notification
    showToast(message, type = 'info') {
        const toast = document.createElement('div');
        toast.className = `toast ${type}`;
        toast.textContent = message;

        this.elements.toastContainer.appendChild(toast);

        setTimeout(() => {
            toast.remove();
        }, 5000);
    }
}

// Inicializar la aplicación cuando el DOM esté listo
document.addEventListener('DOMContentLoaded', () => {
    new PDFChatBot();
});
